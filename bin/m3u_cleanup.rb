#!/usr/bin/ruby
#
# TODO: 
#  * It'd be nice to accept write-in type replacements when a file can't found (prompt the user to type in a path..)
#  * Option parsing
#  * accept multiple playlsits, and one repo. This would allow us to bulk-do these and genwerate a lookup only once..

require 'find'
require 'optparse'

class M3U

  class Entry
    attr_accessor :title, :path, :seconds

    def initialize(path = nil, title = nil, seconds = nil)
      @title, @path, @seconds = title, path, seconds
    end

    def basename
      File.basename path
    end

    def to_s
      "#EXTINF:%d,%s\r\n%s\r\n" % [seconds,title,path]
    end
  end

  M3U_HEADER = /^#EXTM3U/
  ENTRY_INF_PARTS = /#EXTINF:([\d]*),([^\r\n]+)/

  attr_accessor :playlist, :path

  def initialize(m3u_file = nil)
    @path = m3u_file
    @playlist = []
    load! if @path
  end

  def to_s
    "#EXTM3U\r\n"+@playlist.collect{|e| e.to_s}.join
  end

  def save!
    File.open(@path, 'w') {|f| f.write to_s }
  end

  private

  def load!
    m3ulines = File.new(@path, "r").lines.to_a

    raise StandardError, "Invalid m3u file, unrecognized format." unless M3U_HEADER.match m3ulines.shift 

    current_entry = Entry.new
    m3ulines.each do |line|
      case line
        when ENTRY_INF_PARTS
          current_entry.seconds = $1.to_i
          current_entry.title = $2
        else 
          current_entry.path = line.chomp
          @playlist << current_entry
          current_entry = Entry.new
      end
    end
  end
end

# The only two arguments really:
m3u_path   = nil
media_repo = nil

# Parse-time:
option_parser = OptionParser.new do |opts|
  opts.separator " "
  opts.separator "Mandatory options:"

  opts.on("-p", "--playlist M3U-PATH", "Playlist to cleanup") {|o| m3u_path = o }

  opts.on("-r", "--repo REPO-DIR", "Media repository to use when finding missing files") {|o| media_repo = o }

  opts.separator " "
  opts.separator "Other options:"

  opts.on_tail "-h", "--help", "Show this message"
end

option_parser.parse!

if m3u_path.nil? or media_repo.nil?
  puts option_parser.to_s
  exit
end

# Seems good to go - let's try doing some work:

DIRECTORY_SPLIT = /[\/\\]/

m3u = M3U.new m3u_path
puts "- Parsed m3u file successfully"

puts "- Indexing media repository..."
repo_files = ''
Find.find(media_repo){|path| repo_files << path+"\n" unless /^[.+]$/.match path }

puts "- Fixing m3u"
m3u.playlist.each do |entry|
  unless File.exists? entry.path
    new_path = nil
    puts "* Missing Playlist entry: "+entry.path.inspect

    # First we try all the obvious/easy matches by searching for the exact file:
    file_parts = entry.path.split(DIRECTORY_SPLIT).reject{|p| p.length == 0}

    # These are possible search matches in order of most to least specific
    path_fragments = 0.upto(file_parts.length-1).collect{|i| file_parts[i..file_parts.length].join('/')}

    catch (:found_replacement) do 
      already_prompted_paths = []
      [
        # Search for an exact match on the reassembled path fragment
        Proc.new{|s| Regexp.escape s},
        # Search for a match after taking out the crazy characters
        # TODO: Properly Escape actual periods?
        Proc.new{|s| s.gsub(/[^0-9a-zA-Z\/ -\.]/, '.').gsub(' ', '\\ ').gsub('-', "\\-") },
        # Now search for a match after taking out the spaces and dashes too... 
        # Keep in mind we might be dealing with multi-byte chars, so we use {0,2}
        Proc.new{|s| s.gsub(/[^0-9a-zA-Z\/\.]/, '.{0,2}').gsub(' ', '\\ ').gsub('-', "\\-") }
      ].each do |path_transform|
        path_fragments.collect(&path_transform).collect{ |search| repo_files.scan /^(.+#{search})$/ }.flatten.each do |match|
          unless already_prompted_paths.include? match 
            already_prompted_paths << match
            puts  "   Found : "+match.inspect
            print "   Accept replacement? (y/n): "

            entry.path = match and throw :found_replacement if gets.chomp == 'y'
          end
        end
      end
      
      puts "   Couldn't locate an obvious replacement ..."
    end
  end  
end

puts "- Completed playlist cleanup."

print "- Save changes to your playlist? (y/n): "

m3u.save! if gets.chomp == 'y'

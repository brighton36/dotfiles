#!/usr/bin/env ruby


require 'open-uri'
require 'rubygems'

gem 'nokogiri'
require 'nokogiri'

def download_uri_to(uri_source, path_dest)
  filename = File.basename uri_source.path

  open(uri_source) do |source_file| 
    open('%s/%s' % [path_dest,filename], "wb") do |dest_file|
      dest_file.write source_file.read
    end
  end
end

class InvalidUriArg < StandardError;end;

class ArchiveOrgShow
  attr_reader :uri

  def initialize(uri)
    @uri = URI.parse uri
  end

  def audio_files
    flac_files || shn_files || mp3_files
  end

  def shn_files
    @shn_files ||= anchor_uris_matching /\.shn$/i
    (@shn_files.length > 0) ? @shn_files : nil
  end

  def flac_files
    @flac_files ||= anchor_uris_matching /\.flac$/i
    (@flac_files.length > 0) ? @flac_files : nil
  end

  def mp3_files
    @mp3_files ||= anchor_uris_matching /\.mp3$/i
    (@mp3_files.length > 0) ? @mp3_files : nil
  end

  def txt_files
    @txt_files = anchor_uris_matching /\.txt$/i unless @txt_files
    @txt_files
  end

  def as_doc
    @doc = Nokogiri::HTML(open(uri.to_s)) unless @doc
    @doc
  end

  def identifier
    File.basename uri.path
  end

  def valid?
    audio_files.length > 0
  end

  private

  def anchor_uris_matching(against)
    as_doc.css('a').collect{ |node| uri_for node['href'] if against.match node['href'] }.compact
  end
  
  def uri_for(relative_uri)
    new_uri = nil

    case relative_uri
      when /^[^\:]+\:\/\/.+/
        new_uri = URI.parse relative_uri
      when /^\/.+$/
        new_uri = uri.dup
        new_uri.path = relative_uri
      else
        new_uri = uri.dup
        new_uri.path = '%s/%s' % [File.dirname(new_uri.path), relative_uri]
    end

    new_uri
  end
end

if ARGV.length != 2
  puts "%s [archive.org/url] [download-location]" % $0
  exit
end

show = ArchiveOrgShow.new ARGV[0]

raise InvalidUriArg unless show.valid?

destination_path = '%s/%s' % [
  # This removes the trainling slash, if one is present:
  (/^(.+)\/$/.match ARGV[1]) ? $1 : ARGV[1],
  show.identifier
]

puts "Downloading %s to %s" % [show.identifier.inspect,destination_path]
FileUtils.mkdir destination_path unless File.directory? destination_path

(show.txt_files + show.audio_files).each do |u|
  puts "  * Downloading %s" % File.basename(u.path)
  download_uri_to u, destination_path
end

puts "Completed Successfully!"

#!/usr/bin/ruby

# THis is kind of a hack-up run-once thing. We'll take a single param (the mp3 dir) and ouput the minutes of phone time used for each month in that directory.
# It was either this or a bash one-liner . I chose to retain the sanity. Started at 11:15A-12:07p (with ldi/rbensh interuptions)

# TODO: I want average call time per month
# TODO: I want average call time per DOTW
# TODO: PLaced versus received calls?

require 'time'
require 'rubygems'
require "mp3info"

begin puts "Missing path argument: dti_call_time_stats.rb [recording-path]" ; exit end if ARGV.length != 1 or !File.directory? ARGV[0]

# rtrim any trailing '/' in the param:
recording_dir = $1 if /(.*?)[\/]?$/.match ARGV[0]

monthly_times = {}
dotw_times = {}

recordings_parsed = 0
Dir['%s/*.mp3' % recording_dir ].each do |recording|
  if /^#{recording_dir}\/\(([^\)]+)\).+/.match recording
    recordings_parsed += 1
    recorded_on = Date.parse($1)

    # If needed, setup month counters:
    monthly_times[recorded_on.year] = {} unless monthly_times.has_key? recorded_on.year
    monthly_times[recorded_on.year][recorded_on.month] = 0.0 unless monthly_times[recorded_on.year].has_key? recorded_on.month

    # If needed, setup dotw counters:
    dotw_times[recorded_on.year] = {} unless dotw_times.has_key? recorded_on.year
    dotw_times[recorded_on.year][recorded_on.wday] = 0.0 unless dotw_times[recorded_on.year].has_key? recorded_on.wday

    # Recording Time (in seconds)
    recording_duration = Mp3Info.open(recording).length

    # Add time to month's counter:
    monthly_times[recorded_on.year][recorded_on.month] += recording_duration

    # Add time to dotw's counter:
    dotw_times[recorded_on.year][recorded_on.wday] += recording_duration

  else
    puts "Warning: unmatched mp3 file not counted: #{recording.inspect}"
  end

end

# OK - output time!
puts "%d recordings parsed." % recordings_parsed
puts
puts "Monthly Call Time Aggregates"
puts "-----------------------------------"
monthly_times.keys.sort.each do |year|
  monthly_times[year].keys.sort.each do |month|
    puts "%s: %.2f minutes" % [ 
      Time.mktime(year, month).strftime('%b %y'),
      monthly_times[year][month] / 60
    ]
  end
end

# OK - output time!
puts
puts "Monthly DOTW Call Time Aggregates"
puts "-----------------------------------"
dotw_times.keys.sort.each do |year|
  dotw_times[year].keys.sort.each do |wday|
    puts "%s - %s: %.2f minutes" % [ 
      Time.mktime(year).strftime('%y'),
      Time::RFC2822_DAY_NAME[wday],
      dotw_times[year][wday] / 60
    ]
  end
end


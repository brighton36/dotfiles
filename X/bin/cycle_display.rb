#!/usr/bin/ruby
# frozen_string_literal: true

require 'open3'

# A simple abstraction for the output of xrandr info, and, its invocation
class Xrandr
  XRANDR_PATH = '/usr/bin/xrandr'

  def initialize(from_s)
    @from_s = from_s
  end

  def name
    ::Regexp.last_match(1) if /\A([^ ]+)/.match @from_s
  end

  def resolutions
    @from_s.scan(/^[  ]+([^ ]+).+$/).flatten
  end

  def connected?
    !!(@from_s =~ / connected/)
  end

  def primary?
    !!(@from_s =~ / primary/)
  end

  def self.all
    run.scan(/^[^ ].+?(?=\n[^ ])/m).tap(&:shift).map { |line| new line }
  end

  def self.run(*args)
    output, error, status = Open3.capture3 XRANDR_PATH, *args

    # For now, we're trying to debug the cases where this poops out:
    #File.open(format('/home/cderose/tmp/%s.xrandr.out',Time.now.strftime('%Y%m%d_%I%M')),'w').write(output)

    if status.exitstatus != 0 || !error.empty?
      puts format('Fatal Error running xrandr (%<code>d): %<error>s', code: status.exitstatus, error: error)
      exit 1
    end

    output
  end
end

displays = Xrandr.all.find_all(&:connected?)

exit 0 if displays.length < 2

# Determine the new primary:
primary = displays.find { |display| !display.primary? }

# Let's rotate the primary:
Xrandr.run '--output', primary.name, '--mode', primary.resolutions.first, '--auto', '--primary',
  *displays.find_all { |display| display != primary }.map { |display| ['--output', display.name, '--off'] }.flatten

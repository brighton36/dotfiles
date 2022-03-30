#!/usr/bin/env ruby

# NOTE: Here's what we're doing atm:
# 1. scp -r .ssh/ 192.168.122.130:~/
# 2. then ssh into 192.168.122.130
# 3. alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
# 4. git clone --bare git@github.com:brighton36/home.git $HOME/.cfg
# 5. config config --local status.showUntrackedFiles no
# 6. config checkout --force

require 'optparse'
require 'io/console'

# require 'net/ssh'
require 'net/scp'
require 'pastel'

@pastel = Pastel.new
def info(str)
	puts '%s: %s' % [@pastel.green('INFO'), str]
end

def warn(str)
	puts "%s: %s" % [@pastel.yellow('WARN'), str]
end

def error!(str)
	puts "%s: %s" % [@pastel.red('ERROR'), str]
	exit
end

MOVEIN_COMMANDS = [ 
	'alias config=\'/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME\'',
	'git clone --bare git@github.com:brighton36/home.git $HOME/.cfg',
	'config --local status.showUntrackedFiles no',
	'config checkout --force' ]

opt_parser = OptionParser.new do |opts|
  opts.banner = "Usage: %s [username@]host" % $0

  opts.on("-h", "--help", "Prints this help") do
    puts opts
    exit
  end
end

opt_parser.parse!

unless ARGV.length == 1
  puts opt_parser.help
  exit
end

user, host = (/\A([^\@]*)\@(.+)\Z/.match(ARGV[0])) ? 
  [$1, $2] : [ENV['USER'], ARGV[0]]

print "%s@%s's password: " % [user, host]
password = STDIN.noecho(&:gets).chomp
puts

Net::SCP.start(host, user, password: password) do |scp|
	# synchronous (blocking) upload; call blocks until upload completes
	warn "Copying local .ssh to %s/.ssh (make sure you want this)" % [host]
	scp.upload! ".ssh", ".ssh", recursive: true, preserve: true, verbose: true
end

Net::SSH.start(host, user, password: password) do |session|
  session.open_channel do |channel|
    channel.on_data{ |c, data| puts data }
    channel.on_extended_data{ |c, data| puts data }
		channel.on_close { info "channel closed" }
    channel.request_pty do |ch, success|
			error! "Unable to request pty" unless success

      channel.send_channel_request "shell" do |ch, success|
        error! "Unable to open SSH channel" unless success
      end
		end

    # TODO: Why doesn't this wokr...
    MOVEIN_COMMANDS.each do |command|
      info command
      channel.send_data(command+"\n")
    end

  end

end


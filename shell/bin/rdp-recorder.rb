#!/usr/bin/env ruby
=begin

=Remote Desktop Recorder

== TODO:

== DONE:
  - Get compression down even more. Try different codecs. Thanks Chris!
  - Find remote desktop instance.
  - Find WinVNC windows for recording. A new process is not spawned when a session is started also the listener agent is always on.
  - Start recording to tmp directory.
  - Loop and check if there are any orphaned recordings running.
  - Kill recording if it has been orphaned.
  - Popup window asking for client name and client user assisted.
  - Save to samus using the client name and username as file name under timestamped directory.

== Dependancies:
  - apt-get install libnotify-bin ruby-gnome2 ffmpeg2theora
  - sudo gem install RubyInline

=end

require 'rubygems'
require 'activesupport'
require 'fileutils'
require 'tmpdir'

class SessionRecording
  cattr_accessor :logger

  VALID_REMOTE_SESSIONS = /\s?(?>\d*).*(rdesktop|xtightvncviewer|winvnc)/i
  VALID_RECORDING_PROC  = /^\s?([\d]*).*(recordmydesktop).*(workdir)/i
  VALID_USER_ENTRY = /\W+/

  RECORDMYDESKTOP = %w( /usr/bin/recordmydesktop --on-the-fly-encoding --no-frame --no-sound -workdir %s -o %s )
  
  XWININFO = '/usr/bin/xwininfo'
  NOTIFY = '/usr/bin/notify-send'
  SAVE_DIR = '/mnt/samus_monitor/queue'
  CHMOD = '/bin/chmod'
  PS = '/bin/ps'
 
  # Start recording the remote session.
  def start
    timestamp = Time.now.strftime('%m.%d.%Y-%H:%M:%S')
    tmp_dir = Dir.tmpdir
    @tmp_filename = "#{tmp_dir}/#{timestamp}-#{ENV['USERNAME']}.ogv"
    logger.info "Temporary filename: #{@tmp_filename}"
    notify "Found RDP session. Starting recording session."

    # Fork recording process to avoid zombie process' being left behind after death.
    pid = fork { IO.popen RECORDMYDESKTOP.join(' ') % [ tmp_dir, "#{@tmp_filename}" ] }
    Process.detach(pid)
    set_rec_procid
  end
  
  # kill active recording process for current recording session
  def kill
    logger.info "Killing #{@rec_pid}"
    notify "Ending recording session."
    system "kill -9 #{@rec_pid}" 
    gtk_popup
  end
  
  # kill all active recordings. Acts as a failover.
  def kill_all_recordings
    logger.info "Killing all active recordings due to errors."
    notify "Killing all active recordings due to errors!"
    system "killall recordmydesktop"
  end

  def gtk_popup
    pid = fork {  

    require 'gtk2'
      dialog = Gtk::Dialog.new(
          "Edit User Information",
          nil,
          Gtk::Dialog::MODAL,
          [ Gtk::Stock::OK, Gtk::Dialog::RESPONSE_OK ],
          [ Gtk::Stock::CANCEL, Gtk::Dialog::RESPONSE_CANCEL ]
      )
      dialog.default_response = Gtk::Dialog::RESPONSE_OK
      label1 = Gtk::Label.new("Client Name")
      label2 = Gtk::Label.new("User Assisted")

      client = Gtk::Entry.new
      assisted = Gtk::Entry.new

      client.text = ''
      assisted.text = ''
      
      table = Gtk::Table.new(4, 2, false)
      table.attach_defaults(label1, 0, 1, 0, 1)
      table.attach_defaults(label2, 0, 1, 1, 2)
      table.attach_defaults(client,   1, 2, 0, 1)
      table.attach_defaults(assisted,   1, 2, 1, 2)
      table.row_spacings = 5
      table.column_spacings = 5
      table.border_width = 10

      dialog.vbox.add(table)
      dialog.show_all

      # Run the dialog and output the data if user okays it
      dialog.run do |response|
        if response == Gtk::Dialog::RESPONSE_OK
          # Validate User input from pop up dialog
          valid_cust_info = "#{client.text.downcase.gsub(VALID_USER_ENTRY, '')}-#{assisted.text.downcase.gsub(VALID_USER_ENTRY, '')}"
          # Retrieve just the filename from the temporary file
          parsed_tmp_filename = File.basename( @tmp_filename.gsub( File.extname(@tmp_filename), '' ) )
          # Rename the file to finalized filename incase the final move fails.
          new_temp_file = "#{File.dirname(@tmp_filename)}/#{parsed_tmp_filename}-#{valid_cust_info}.ogv"
          logger.debug "New temp file: #{new_temp_file}"
          notify "Failed to rename #{@tmp_filename}." unless
            FileUtils.mv @tmp_filename, new_temp_file
          # Move to final location.
          final_save_filename = "#{SAVE_DIR}/#{parsed_tmp_filename}-#{valid_cust_info}.ogv"
          logger.debug "Final save file: #{final_save_filename}"
          notify "Failed to move #{final_save_filename} to server." unless
            FileUtils.copy new_temp_file, final_save_filename 
        end
      end
      dialog.destroy   
    }
    Process.detach(pid)
    set_rec_procid
  end
  
  # Find a recordable window. Get process ID of remote window also.
  def self.recordable_windows
    %x["#{XWININFO}" -root -tree].match VALID_REMOTE_SESSIONS
    $1
  end

  private

  def set_rec_procid
    %x["#{PS}" -U "#{ENV['USERNAME']}" -o '%p %a'].match VALID_RECORDING_PROC
    @rec_pid = $1
    logger.info "Recording process id: #{@rec_pid}"
  end
  
  def notify( message, title="RDP Recorder" )
    system "#{NOTIFY} -i notification-display-brightness-full.svg '#{title}' '#{message}'"
  end
   
  def logger
    self.class.logger
  end
  
end

# Lets start monitoring remote sessions, recording, and killing them!
current_recording = nil
SessionRecording.logger = Logger.new(STDOUT)

loop do
  recordable_window = SessionRecording.recordable_windows
  if current_recording.nil?
    # check for recordable windows.
    if recordable_window
      current_recording = SessionRecording.new
      current_recording.start
    end
  elsif current_recording and !recordable_window
      current_recording.kill
      current_recording = nil
  end
  sleep 3
end

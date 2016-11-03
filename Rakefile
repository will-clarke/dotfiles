require 'fileutils'
require 'pathname'

# Installs / Uninstalls / Backs Up all dotfiles
class Installer
  EXCLUDED_FILES = [
    'Rakefile', 'readme.md', 'README.md',
    'setup_script_from_thoughtbot', 'install.sh',
    'applications'
  ]

  def repo(name)
    File.expand_path name
  end

  def destination(name)
    File.expand_path "~/.#{name}"
  end

  def install
    dotfiles.each do |name|
      if File.exist?(destination(name))
        p "Exists:   #{destination name}"
      else
        p "LINKING:  #{destination(name)}"
        link(repo(name), destination(name))
      end
    end
    Dropbox.new.link_secrets_from_dropbox
    Applications.new.link_source
  end

  def backup(file_names = dotfiles)
    file_names.each do |name|
      create_backup_folder name
      unless File.directory? destination(name)
        file_exists = File.exist? destination(name)
        FileUtils.cp destination(name), file_path if file_exists
      end
    end
  end

  def create_backup_folder(name)
    file_path = File.join backup_folder.first.to_s, name
    directory = File.directory?(destination(name))
    ensure_folder_has_been_created file_path, directory
  end

  def backup_folder
    time = Time.now.strftime '%y-%m-%d---%H-%M'
    file_path = File.join('~', '.dotfiles-backup', time)
    FileUtils.mkdir_p(File.expand_path file_path)
  end

  def ensure_folder_has_been_created(path, dir = false)
    directory = dir ? path : path.split('/')[0..-2].join('/') + '/'
    FileUtils.mkdir_p repo(directory)
  end

  def dotfiles
    Dir['*'] - EXCLUDED_FILES
  end

  def link(repo, destination)
    FileUtils.ln_s(repo, destination) unless File.symlink? repo
  end

  def uninstall
    dotfiles.each do |file|
      backup [destination(file)]
      FileUtils.rm_rf destination file
      p "REMOVING: #{destination file}]"
    end
    Dropbox.new.unlink_secrets_from_dropbox
    Applications.new.delete_source
  end

  def overwrite
    backup
    uninstall
    install
  end
end

# link from ~/Dropbox/dev/secrets to ~/.secret_file
class Dropbox
  def link_secrets_from_dropbox
    `ln -s ~/Dropbox/org/ ~/org/`
    each_secret_file_and_destination do |secret_file, destination|
      if ::Pathname.new(destination).exist?
        p "Exists:   #{destination} [SECRET]"
      elsif ::Pathname.new(secret_file).exist?
        FileUtils.ln_s(secret_file, destination)
        p "LINKING:  #{destination} [SECRET]"
      else
        p "Can't find ~/Dropbox/dev/secrets"
      end
    end
  end

  def unlink_secrets_from_dropbox
    `rm ~/org`
    each_secret_file_and_destination do |secret_file, destination|
      if ::Pathname.new(destination).exist?
        FileUtils.rm_rf(destination)
        p "REMOVING: #{destination} [SECRET]"
      else
        p "Removed:  #{destination} [SECRET]"
      end
    end
  end

  def each_secret_file_and_destination(&block)
    dropbox_location = "#{ENV['HOME']}/Dropbox/dev/secrets"
    (p 'Error:   Dropbox not linked' && return) unless Dir.exist?(dropbox_location)
    ::Pathname.new(dropbox_location).children
      .reject { |i| i.to_s =~ /DS_Store|gitconfig/ }
      .each do |source|
      destination = "#{ENV['HOME']}/.#{source.split.last}"
      block.call source, destination
    end
  end
end

# Link files in ~/dotfiles/applications to proper places
class Applications
  def location_hash
    {
      "#{ENV['HOME']}/dotfiles/applications/karabiner/private.xml" =>
      "#{ENV['HOME']}/Library/Application Support/Karabiner/private.xml"
    }
  end

  def link_source
    each_source_and_destination do |source, destination|
      begin
        if source.exist? && !destination.exist? && source.dirname.exist?
          FileUtils.ln_s source, destination
          p "LINKING:  #{destination} [SECRET]"
        elsif destination.exist?
          p "Exists:   #{destination}"
        end
      rescue => e
        p "Error trying to link #{source} & #{destination}"
        p e
      end
    end
  end

  def ask_to_remove file
    p "Remove #{file}?"
    p '[yes/no/diff]'
    overwrite = STDIN.gets.chomp
    if overwrite == 'y' || overwrite == 'yes'
      p "REMOVING: #{file}"
      FileUtils.rm file
    elsif overwrite == 'diff'
      p file.read
      ask_to_remove file
    else
      p "Not overwriting #{file}"
    end
  end

  def delete_source
    each_source_and_destination do |source, destination|
      if destination.exist?
        p ''
        p "#{destination} exists."
        ask_to_remove destination
      end
    end
  end

  def each_source_and_destination(&block)
    location_hash.each do |from, to|
      source = ::Pathname.new(from)
      destination = ::Pathname.new(to)
      block.call source, destination
    end
  end
end

desc 'Install'
task :install do
  Installer.new.install
end

desc 'Backup'
task :backup do
  Installer.new.backup
end

desc 'Update / Overwrite dotfiles repository.'
task :update do
  system 'git pull'
  Installer.new.overwrite
  system 'vim +PluginInstall! +qall'
  p 'Update completed successfully!'
end

desc 'Overwrite: will delete vim bundles'
task :overwrite do
  Installer.new.overwrite
end

desc 'Completely remove all dotfiles'
task :uninstall do
  Installer.new.uninstall
end

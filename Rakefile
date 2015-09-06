require 'fileutils'
require 'pathname'

# Installs / Uninstalls / Backs Up all dotfiles
class Installer
  EXCLUDED_FILES = [
    'Rakefile', 'readme.md', 'README.md',
    'setup_script_from_thoughtbot', 'install.sh',
    'applications',
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
        p "#{destination name} exists"
      else
        p "Linking #{destination(name)}"
        link(repo(name), destination(name))
      end
    end
    link_secrets_from_dropbox
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
    end
  end

  def link_secrets_from_dropbox
    dropbox_location = ENV['HOME'] + '/Dropbox/dev/secrets'
    fail 'Dropbox not linked' unless Dir.exist?(dropbox_location)

    directories, files = ::Pathname.new(dropbox_location)
      .children.group_by { |f| f.directory? }
      .sort_by { |k,v| k ? 0 : 1 }
      .map{|f| f[1].map {|i| i.to_s } }

    directories.each do |directory|
      name = directory.split('/').last
      local_path = ENV['HOME'] + '/.' + name
      if Dir.exist?(local_path)
        p "Secret Directory ~/.#{name} already exists"
      else
        FileUtils.ln_s(directory, local_path)
        p "Linking Secret Directory #{name}"
      end
    end

    files.reject { |i| i.match(/DS_Store|gitconfig/) }.each do |file|
      name = file.split('/').last
      local_path = ENV['HOME'] + '/.' + name
      if File.exist?(local_path)
        p "Secret File ~/.#{name} already exists"
      else
        FileUtils.ln_s(file, local_path)
        p "Linking Secret File #{name}"
      end
    end
  end

  def overwrite
    backup
    uninstall
    install
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

require 'fileutils'

class Installer
  def install
    dotfiles.each do |src|
      dest = "~/.#{src}"

      source = File.expand_path(src)
      destination = File.expand_path(dest)

      if File.exist?(destination)
        puts "#{dest} exists"
      else
        puts "Linking #{dest}"
        link(source, destination)
      end
    end
  end

  def overwrite
    dotfiles.each do |src|
      dest = "~/.#{src}"

      source = File.expand_path(src)
      destination = File.expand_path(dest)

      if File.exist?(destination)
        backup [src]
        puts "#{dest} exists. Backed up & linked."
        begin
        FileUtils.rm_rf(destination)
        rescue => e
          p "ERROR: #{e}"
        end
      end

        link(source, destination)
    end
  end

  def backup_folder
    time = Time.now.strftime "%y-%m-%d--%R"
    file_path = File.join( '~' , '.dotfiles-backup', time )
    FileUtils.mkdir_p( File.expand_path file_path)
  end

  def backup(file_names=dotfiles)
    file_names.each do | file |
      original_file = File.expand_path( "~/.#{file}" )
      if File.directory? original_file
        original_file += '/.'
      end
      backup_destination = File.expand_path(File.join("#{backup_folder.first}", "#{file}"))
      FileUtils.cp_r original_file, backup_destination if File.exists? file
    end
  end

  def dotfiles
    Dir['*'] - ['Rakefile', 'README.md']
  end

  def link(source, destination)
    FileUtils.ln_s(source, destination)
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

desc 'Update dotfiles repository.'
task :update do
  system 'git pull'
  system 'vim +PluginInstall! +qallpry'
  Installer.new.install
end

desc 'Overwrite'
task :overwrite do 
  Installer.new.overwrite
end

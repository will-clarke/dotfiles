require 'fileutils'

class Installer
  EXCLUDED_FILES = /bundle/

  def repo name
    File.expand_path name
  end

  def destination name
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
  end

  def overwrite
    dotfiles.each do |name|

      if File.exist? destination(name) && File.file?(destination(name))
        backup [destination(name)]
        p "#{destination(name)} exists. Backed up & linked."

        begin
          FileUtils.rm_rf(destination(name))
        rescue => e
          p "ERROR: #{e}"
        end
      end

      link(repo(name), destination(name))
    end
  end

  def backup_folder
    time = Time.now.strftime "%y-%m-%d---%H-%M"
    file_path = File.join( '~' , '.dotfiles-backup', time )
    FileUtils.mkdir_p( File.expand_path file_path)
  end

  def ensure_folder_has_been_created path
    directory = path.split('/')[0..-2].join('/')
    FileUtils.mkdir_p repo(directory)
  end

  def backup(file_names=dotfiles)
    file_names.each do | name |
      file_path = File.join backup_folder.first.to_s, name
      ensure_folder_has_been_created destination file_path
      begin
      FileUtils.cp destination(name), file_path if File.exists? destination(name)
      p 'SUCCESS!'
      rescue => e
        p "Couldn't move #{name}"
        p "Permissions error?"
        # require 'pry'; binding.pry
      end
    end
  end

  def convert_files_to_repo_path files
    array = files.class == Array ? files : [files]
    array.map{|i| i.split('.')[1..-1].join('.')}.map{|i| repo i}
  end

  def source_to_repo
    directories = Dir['*'].select{ |i| File.directory? i }
    directories.each do |directory|
      local_directory_tree = Dir[destination(directory) + '/**/*']
      repo_directory_tree = Dir[repo(directory) + '/**/*']
      filess_to_exclude = local_directory_tree.select{|i| i=~EXCLUDED_FILES}
      local_directory_tree = local_directory_tree - filess_to_exclude

      # Work out the files we don't have in the repo
      files_to_upload_to_repo = convert_files_to_repo_path(local_directory_tree)
        .each_with_index.map do |repo_path, index|
        [repo_path, local_directory_tree[index]]
      end.select{|r, l| !File.exists? r}.map{|i| i[1]}

      # Iterate through directories & create them in repo
      dotfiles_directories = files_to_upload_to_repo.select{|i| File.directory? i}
      convert_files_to_repo_path(dotfiles_directories).each{|dir| FileUtils.mkdir_p dir }

      # Iterate through files & copy them accross
      files = files_to_upload_to_repo.select{|i| File.file? i}
      files.each do |dotfiles_file|
        unless File.symlink? dotfiles_file
          p "Copying from #{dotfiles_file}"
          FileUtils.cp dotfiles_file, convert_files_to_repo_path(dotfiles_file)[0] if File.exists? dotfiles_file
        end
      end
    end
  end

  def dotfiles
    files = Dir['*'] + Dir['**/*'] - ['Rakefile', 'README.md']
    files.select{|i| i !~ EXCLUDED_FILES}
  end

  def link(repo, destination)
    if File.symlink? repo
      p 'OMG. symlink'
    end
    FileUtils.ln_s(repo, destination)
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
  Installer.new.overwrite
end

desc 'Overwrite'
task :overwrite do
  Installer.new.overwrite
end

desc 'Copy any new files within predefined ~/. directories to repo'
task :source_to_repo do
  Installer.new.source_to_repo
end

desc 'Hard Upload & Overwrite/Update'
task :source_to_repo_and_overwrite do
  Installer.new.source_to_repo
  Rake::Task[:update].invoke
end

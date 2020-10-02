
task default: [:package]

task package: [:build_frontend, :build_backend] do
  def read_cabal_package_version
    version_line = `grep '^\s*version' twitanalysis.cabal`.chomp
    return /:\s*([0-9a-zA-Z.]+)$/.match(version_line.chomp)[1]
  end
  puts "Hello, Rake is running: build_backend"
  out_tar_filename = Dir.chdir 'backend-app/' do
    # Define some variables:
    version = read_cabal_package_version
    commit = `git rev-list HEAD | head -n 1`.chomp
    local_install_root = `stack path --local-install-root`.chomp
    out_tar_filename = "twitanalysis-#{version}-#{commit[...8]}.bdist.tar.gz"
    # Log some output:
    puts "The cabal package is in version: #{version}"
    puts "Naming the binary package: #{out_tar_filename}"
    # Do file operations:
    if File.exist? out_tar_filename
      File.delete(out_tar_filename)
    end
    FileUtils.cp_r('static', local_install_root, remove_destination: true) # Just shove it in
    system("tar -C '#{local_install_root}' -czvf '#{out_tar_filename}' .")
    File.rename(out_tar_filename, '../' + out_tar_filename)
    out_tar_filename
  end
  symlink_name = 'twitanalysis.bdist.tar.gz'
  if File.exist? symlink_name
    File.delete(symlink_name)
  end
  File.symlink(out_tar_filename, symlink_name)
  puts "Backend app built; please look at this file: #{symlink_name}"
end

task build_frontend: [:install_frontend_deps] do
  Dir.chdir 'frontend-app/' do
    system 'yarn build'
  end
end

task build_backend: [:install_backend_deps] do
  Dir.chdir 'backend-app/' do
    system 'stack build'
  end
end

task :install_backend_deps do
  # check for stack; install if missing
  if system 'which stack'
    puts "Your system already has Haskell Stack"
  else
    # https://docs.haskellstack.org/en/stable/install_and_upgrade/
    puts 'Apparently requires sudo?'
    sytem 'curl -sSL https://get.haskellstack.org/ | sh'
  end
end

task :install_frontend_deps do
  if system 'which yarn'
    puts "Your system already has Yarn installed."
  else
    puts "Sorry we don't know how to install Yarn."
    exist 1
  end
end

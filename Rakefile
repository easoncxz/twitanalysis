
task default: [:build]

task build: [:build_backend] do
end

task build_backend: [:install_backend_deps] do
  def read_cabal_package_version
    version_line = `grep '^\s*version' twitanalysis.cabal`.chomp
    return /:\s*([0-9a-zA-Z.]+)$/.match(version_line.chomp)[1]
  end
  puts "Hello, Rake is running: build_backend"
  out_tar_filename = Dir.chdir 'backend-app/' do
    system 'stack build'
    version = read_cabal_package_version
    commit = `git rev-list HEAD | head -n 1`.chomp
    out_tar_filename = "twitanalysis-backend-#{version}-#{commit[...8]}.bdist.tar.gz"
    puts "The cabal package is in version: #{version}"
    puts "Naming the binary package: #{out_tar_filename}"
    if File.exist? out_tar_filename
      File.delete(out_tar_filename)
    end
    system('tar -C "$(stack path --local-install-root)"' + " -czvf '#{out_tar_filename}' .")
    File.rename(out_tar_filename, '../' + out_tar_filename)
    out_tar_filename
  end
  symlink_name = 'twitanalysis-backend.bdist.tar.gz'
  if File.exist? symlink_name
    File.delete(symlink_name)
  end
  File.symlink(out_tar_filename, symlink_name)
  puts "Backend app built; please look at this file: #{symlink_name}"
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

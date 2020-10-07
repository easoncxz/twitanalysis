
require 'octokit'

task default: [:package]

# Args and deps:
#     https://stackoverflow.com/questions/5736786/how-do-you-declare-a-rake-task-that-depends-on-a-parameterized-task
task :publish, [:tag] => [:package] do |t, args|
  # Define variables:
  repo = 'easoncxz/twitanalysis'
  git_tag = args[:tag] || begin
    puts "Which version to publish? Pass a Git tag: rake publish GIT_TAG"
    Process.exit 1
  end
  tarball = `realpath built-packages/twitanalysis.bdist.tar.gz`.chomp
  puts "Working with repo: #{repo}"
  puts "Working with tag: #{git_tag}"
  puts "Reading this bdist tarball: #{tarball}"
  # Do stuff:
  token = ENV['GITHUB_OAUTH_TOKEN']
  unless token
    puts "Error: We need a Github OAuth 2.0 access token in GITHUB_OAUTH_TOKEN."
    Process.exit 1
  end
  client = Octokit::Client.new(:access_token => token)
  release =
    begin
      # All possible client errors:
      #   https://octokit.github.io/octokit.rb/Octokit/ClientError.html
      puts "Trying to fetch release: #{git_tag}"
      client.release_for_tag(repo, git_tag)
    rescue Octokit::NotFound => e
      puts "Trying to create release: #{git_tag}"
      client.create_release(repo, git_tag)
    end
  puts "Using this release: #{release.html_url}" # https://developer.github.com/v3/repos/releases/#list-releases
  online =
    begin
      existing_assets = client.release_assets(release.url)
      bdist_found = existing_assets.select do |a|
        a.name =~ /^twitanalysis.*\.bdist\.tar\.gz$/
      end.first
      if bdist_found
        bdist_found
      else
        client.upload_asset(release.url, tarball, content_type: 'application/gzip')
      end
    end
  puts "File now online: #{online.name}"
  puts "This file's URL: #{online.browser_download_url}"  # https://developer.github.com/v3/repos/releases/#list-release-assets
  puts "Release URL: #{release.html_url}"
end

task package: [:build_frontend, :build_backend] do
  BUILT_PACKAGES_DIR = 'built-packages'
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
    platform = [
      `uname -m`.chomp, # x86_64
      '-',
      `uname -s`.chomp, # Darwin
      '-',
      `uname -r`.chomp, # 17.7.0
    ].join('')
    out_tar_filename = "twitanalysis-#{version}-#{commit[...8]}-#{platform}.bdist.tar.gz"
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
  symlink_name = Dir.chdir BUILT_PACKAGES_DIR do
    FileUtils.mv("../#{out_tar_filename}", out_tar_filename)
    symlink_name = 'twitanalysis.bdist.tar.gz'
    if File.exist? symlink_name
      File.delete(symlink_name)
    end
    File.symlink(out_tar_filename, symlink_name)
    symlink_name
  end
  puts "Backend app built. Please look at this symlink: #{BUILT_PACKAGES_DIR}/#{symlink_name}"
end

task build_frontend: [:install_frontend_deps] do
  Dir.chdir 'frontend-app/' do
    system 'yarn install'
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

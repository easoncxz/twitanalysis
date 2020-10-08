
require 'json'
require 'octokit'

def die(*args, **kwargs)
  system(*args, **kwargs) || exit!(1)
end

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
  unless File.exist? tarball
    puts "Error: package not yet built: #{tarball}"
    puts "Run `rake package` first."
    Process.exit 1
  end
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

task package: [:build_frontend, :test_fronend, :build_backend, :test_backend] do
  puts "Hello, Rake is running: package"
  # Define some variables:
  built_packages_dir = 'built-packages'
  cabal_version =
    begin
      version_line = `grep '^\s*version' backend-app/twitanalysis.cabal`.chomp
      /:\s*([0-9a-zA-Z.]+)$/.match(version_line.chomp)[1]
    end
  npm_version =
    begin
      j = JSON.parse(File.read('frontend-app/package.json'))
      j['version']
    end
  commit_hash = `git rev-list HEAD | head -n 1`.chomp
  platform = [
    `uname -m`.chomp, # x86_64
    `uname -s`.chomp, # Darwin
    # `uname -r`.chomp, # 17.7.0
  ].join('-')
  tar = Dir.chdir 'backend-app/' do
    # Define some variables:
    local_install_root = `stack path --local-install-root`.chomp
    out_tar_filename = "twitanalysis-#{cabal_version}-#{npm_version}-#{commit_hash[...7]}-#{platform}.bdist.tar.gz"
    # Log some output:
    puts "The cabal package has version: #{cabal_version}"
    puts "The npm package has version: #{npm_version}"
    puts "Naming the binary package: #{out_tar_filename}"
    # Do file operations:
    if File.exist? out_tar_filename
      File.delete(out_tar_filename)
    end
    # (Following the `static` symlink)
    FileUtils.cp_r('static', local_install_root, remove_destination: true) # Just shove it in
    system("tar -C '#{local_install_root}' -czvf '#{out_tar_filename}' .")
    FileUtils.mv(out_tar_filename, '..')
    out_tar_filename
  end
  unless Dir.exist? built_packages_dir
    Dir.mkdir built_packages_dir
  end
  FileUtils.mv(tar, built_packages_dir)
  sym = Dir.chdir built_packages_dir do
    symlink_name = 'twitanalysis.bdist.tar.gz'
    FileUtils.ln_s(tar, symlink_name, force: true)
    symlink_name
  end
  puts "Backend app built. Please look at this symlink: #{built_packages_dir}/#{sym}"
end

task build_backend: [:install_backend_tools] do
  Dir.chdir 'backend-app/' do
    die 'stack build --no-terminal'
  end
end

task build_frontend: [:install_frontend_tools] do
  Dir.chdir 'frontend-app/' do
    die 'yarn install'
    die 'yarn build'
  end
end

task test: [:test_fronend, :test_backend]

task test_backend: [:build_backend] do
  Dir.chdir 'backend-app/' do
    die 'stack test --no-terminal'
  end
end

task test_fronend: [:install_frontend_tools] do
  Dir.chdir 'frontend-app/' do
    die 'yarn test'
  end
end

task :install_backend_tools do
  # check for stack; install if missing
  if `which stack`.chomp.empty?
    # https://docs.haskellstack.org/en/stable/install_and_upgrade/
    puts 'Installing Haskell Stack... (It allegedly requires sudo?)'
    system 'curl -sSL https://get.haskellstack.org/ | sh'
  end
end

task :install_frontend_tools do
  if `which yarn`.chomp.empty?
    # https://classic.yarnpkg.com/en/docs/install/
    puts 'Installing Yarn...'
    system 'curl -o- -L https://yarnpkg.com/install.sh | bash'
  end
  ## Don't demand nodenv. It's super slow on CI, and Github Actions
  ## already has a mechanism to set up specified NodeJS versions via
  ## the `actions/setup-node` Action.
  #if `which nodenv`.chomp.empty?
  #  puts "Installing Nodenv using nodenv-installer... (will it work?)"
  #  system 'curl -fsSL https://raw.githubusercontent.com/nodenv/nodenv-installer/master/bin/nodenv-installer | bash'
  #  puts "Setting PATH env for this process..."
  #  home = ENV['HOME']
  #  nodenv_path = ["#{home}/.nodenv/shims", "#{home}/.nodenv/bin"]
  #  ENV['PATH'] = nodenv_path.join(':') + ':' + ENV['PATH']
  #  system 'which nodenv'
  #  system 'which node'
  #  Dir.chdir 'frontend-app/' do
  #    system 'nodenv install --skip-existing'
  #  end
  #end
end

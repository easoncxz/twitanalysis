
task default: [:build]

task :install_backend_deps do
  # check for stack; install if missing
end

task build_backend: [:install_backend_deps] do
  puts "Hello, Rake is running."
  Dir.chdir 'backend-app/' do
    system 'stack build'
  end
end

task build: [:build_backend] do
end

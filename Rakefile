require 'open3'
require 'pathname'
require 'yaml'

require 'rubocop/rake_task'

RUBY_VERSIONS = YAML.load(File.read('.travis.yml'))['rvm']

task default: [:test, :lint]

task :test do
  sh 'bin/smoke --color bin/smoke test'
end

task :test_all_ruby_versions do
  RUBY_VERSIONS.each do |version|
    sh "rvm #{version}@smoke --create do bundle exec rake"
  end
end

task publish: [:test] do
  sh 'docker build --pull --tag=samirtalwar/smoke .'
  sh 'docker push samirtalwar/smoke'
end

RuboCop::RakeTask.new

task lint: :rubocop

task :bless do
  Pathname.glob('test/*.args').each do |args_file|
    basename = args_file.sub_ext('')
    out_file = basename.sub_ext('.out')
    err_file = basename.sub_ext('.err')
    status_file = basename.sub_ext('.status')

    command = ['bin/smoke'] + args_file.readlines.map(&:strip)
    Open3.popen3(*command) do |stdin, stdout, stderr, wait_threads|
      stdin.close_write
      out = stdout.read
      err = stderr.read
      status = wait_threads.value.exitstatus

      out_file.write(out) unless out.empty?
      err_file.write(err) unless err.empty?
      status_file.write("#{status}\n") unless status == 0
    end
  end
end

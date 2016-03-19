require 'open3'
require 'pathname'

task :default => :test

task :test do
  sh 'bin/smoke --color bin/smoke test'
end

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

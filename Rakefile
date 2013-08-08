require 'rake/clean'
require 'rake/testtask'
require 'thread'

class IO
  alias_method :old_puts, :puts

  def puts(s)
    @semaphore ||= Mutex.new
    @semaphore.synchronize do
      old_puts s
    end
  end

end

PARSER = FileList['bootstrap/**/*']
SRC = FileList['src/**/*.mule']
CLEAN.include 'build'

SRC.each do |mule_file|
  lisp_file = mule_file.pathmap('%{^src,build}X.lisp')

  directory lisp_file.pathmap('%d')

  file lisp_file => [mule_file, lisp_file.pathmap('%d')] + PARSER do
    sh "python bootstrap/muleparser.py #{mule_file} #{lisp_file}"
  end

  multitask :default => lisp_file
end

Rake::TestTask.new do |t|
  t.libs = ["lib"]
  t.warning = true
  t.verbose = true
  t.test_files = FileList['spec/*_spec.rb']
end

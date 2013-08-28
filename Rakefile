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

CLEAN.include 'build', 'mule.core'

PARSER = FileList['bootstrap/**/*']
SRC = FileList['src/**/*.mule']
LISP = SRC.pathmap('%{^src,tmp}X.lisp')


SRC.each do |mule_file|
  lisp_file = mule_file.pathmap('%{^src,build}X.lisp')

  directory lisp_file.pathmap('%d')

  file lisp_file => [mule_file, lisp_file.pathmap('%d')] + PARSER do
    sh "python bootstrap/muleparser.py #{mule_file} #{lisp_file}"
  end

  multitask :lisp_files => lisp_file
end

file "mule.core" => :lisp_files do
  sh "sbcl --script build-core.lisp"
end

task :default => "mule.core"

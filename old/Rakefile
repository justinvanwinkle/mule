require 'rake/clean'
require 'rake/testtask'
require 'thread'

verbose(true)

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
LIB = FileList['src/lib/**/*.mule']
TEST = FileList['src/test/**/*.mule']
LISP_LIB = LIB.pathmap('%{^src,build}X.lisp')
LISP_TEST = TEST.pathmap('%{^src,build}X.lisp')
CORE = "mule.core"

CLEAN.include 'build', 'mule.core'

LIB.each do |mule_file|
  lisp_file = mule_file.pathmap('%{^src,build}X.lisp')

  directory lisp_file.pathmap('%d')

  file lisp_file => [mule_file, lisp_file.pathmap('%d')] + PARSER do
    sh "python bootstrap/muleparser.py #{mule_file} #{lisp_file}"
  end

  multitask :lib_files => lisp_file
end

TEST.each do |mule_file|
  lisp_file = mule_file.pathmap('%{^src,build}X.lisp')

  directory lisp_file.pathmap('%d')

  file lisp_file => [mule_file, lisp_file.pathmap('%d')] + PARSER do
    sh "python bootstrap/muleparser.py #{mule_file} #{lisp_file}"
  end

  multitask :test_files => lisp_file
end

file CORE => LISP_LIB + ["build-core.lisp"] do
  sh "sbcl --script build-core.lisp"
end


task :default => [:test_files, CORE]

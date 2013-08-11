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

CLEAN.include 'build', 'tmp'

PARSER = FileList['bootstrap/**/*']
SRC = FileList['src/**/*.mule']
LISP = SRC.pathmap('%{^src,tmp}X.lisp')
PRETTY = SRC.pathmap('%{^src,build}X.lisp')


SRC.each do |mule_file|
  tmp_lisp_file = mule_file.pathmap('%{^src,tmp}X.lisp')
  pretty_file = tmp_lisp_file.pathmap('%{^tmp,build}X.lisp')

  directory tmp_lisp_file.pathmap('%d')
  directory pretty_file.pathmap('%d')

  file tmp_lisp_file => [mule_file, tmp_lisp_file.pathmap('%d')] + PARSER do
    sh "python bootstrap/muleparser.py #{mule_file} #{tmp_lisp_file}"
  end

  file pretty_file => [tmp_lisp_file, pretty_file.pathmap('%d')] + PARSER do
    sh "sbcl --script bin/pretty-print #{tmp_lisp_file} #{pretty_file}"
  end

  multitask :default => pretty_file
end

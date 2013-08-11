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
CLEAN.include 'build', 'tmp'

SRC.each do |mule_file|
  lisp_file = mule_file.pathmap('%{^src,build}X.lisp')

  directory lisp_file.pathmap('%d')

  file lisp_file => [mule_file, lisp_file.pathmap('%d')] + PARSER do
    sh "python bootstrap/muleparser.py #{mule_file} #{lisp_file}"
  end

  multitask :default => lisp_file
end


# SRC.each do |mule_file|
#   tmp_lisp_file = mule_file.pathmap('%{^src,tmp}X.lisp')
#   pretty_file = tmp_lisp_file.pathmap('%{^tmp,build}X.lisp')

#   directory tmp_lisp_file.pathmap('%d')
#   directory pretty_file.pathmap('%d')

#   file tmp_lisp_file => [mule_file, tmp_lisp_file.pathmap('%d')] + PARSER do
#     sh "python bootstrap/muleparser.py #{mule_file} #{tmp_lisp_file}"
#   end

#   file pretty_file => [tmp_lisp_file, pretty_file.pathmap('%d')] + PARSER do
#     sh "sbcl --script bin/pretty-print #{tmp_lisp_file} #{pretty_file}"
#   end

#   multitask :pretty => pretty_file
# end



# Rake::TestTask.new do |t|
#   t.libs = ["lib"]
#   t.warning = true
#   t.verbose = true
#   t.test_files = FileList['spec/*_spec.rb']
# end

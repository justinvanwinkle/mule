require 'rake/clean'

SRC = FileList['src/**/*.mule']
CLEAN.include 'build'

SRC.each do |mule_file|
  lisp_file = mule_file.pathmap('%{^src,build}X.lisp')
  directory lisp_file.pathmap('%d')
  file lisp_file => [mule_file, lisp_file.pathmap('%d')] do
    sh "python bootstrap/muleparser.py #{mule_file} #{lisp_file}"
  end
  task :default => lisp_file
end

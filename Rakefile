require 'rake/clean'

SRC = FileList['src/**/*.mule']
CLEAN.include 'build', 'tmp'

SRC.each do |mule_file|
  lisp_file = mule_file.pathmap('%{^src,tmp}X.lisp')
  pretty_file = lisp_file.pathmap('%{^tmp,build}X.lisp')

  directory lisp_file.pathmap('%d')
  directory pretty_file.pathmap('%d')

  file lisp_file => [mule_file, lisp_file.pathmap('%d')] do
    sh "python bootstrap/muleparser.py #{mule_file} #{lisp_file}"
  end

  file pretty_file => [lisp_file, pretty_file.pathmap('%d')] do
    sh "sbcl --script bin/pretty-print #{lisp_file} #{pretty_file}"
  end
  multitask :default => pretty_file
end

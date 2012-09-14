for f in test_cases/*py;
do
		echo "=============================================="
    echo "Compiling and running $f";
		echo "=============================================="
		python3 compile.py $f > /tmp/`basename $f .py`.lisp
		sbcl --script /tmp/`basename $f .py`.lisp 2>&1
done;

#
# This program includes all of the features 
# of the first prototype of the language
#
# - literal integers (no floats yet)
# - the four arithmetic binary ops +, -, *, /
# - function defs with fixed number of positional parameters
# - arithmetic expressions including parens
# - function calls
# - print as a function
# - looping with "dotimes x 10"
# 
# Specifically the following are not supported in
# reference 1:
# - no floats
# - no list or dict literals
# - no arglist expansion
# - no classes, decorators, iterators
#
# Other limitations:
# - SBCL's print is weird so print only
# supports a single arg for now
# - Compiler is line oriented so all
# statements must be on a single line
# and only one statement per line

# This is another comment


# Simple expressions are supported
1

1 + 1

# print is a function as in Python 3
print(1 + 1)


# We can define simple functions
def mysquare(x):
    return x * x
end
# Note the 'end' keyword

# Functions can have any number of args
# but keyword args are not yet supported
def foo(x, y, z):
    return x + y * z
end


# The only iteration that is supported
# right now is dotimes
dotimes i 10:
    print(mysquare(i))
end
# Note that loops also take the 'end' reserved word


#include <iostream>


class PyObj {

};


union py_number {
    long n;
    double d;
};

template <class T>
class PyNumber : public PyObj {
protected:

public:
    T value;

    PyNumber(T val=0) {
        value = val;
    }

    virtual PyNumber<double> __add__(PyNumber<double>* other) {
        return PyNumber<double>(other->value + value);
    }

    virtual PyNumber<T> __add__(PyNumber<long>* other) {
        return PyNumber<T>(other->value + value);
    }

    virtual ~PyNumber() {}

};


int main() {

    auto p1 = new PyNumber<double>(3.3);
    auto p2 = new PyNumber<long>(4);

    std::cout << p1->__add__(p2).value << std::endl;


    delete p1;
    delete p2;
}


PyNumber<>* add(PyNumber* a, PyNumber* b) {
    return a->__add__(b);
}

//#include <iostream>
#include <atomic>

class PyObj {
    std::atomic<long> refcount {0};

public:
    long incr() {
        return ++refcount;
    }

    long decr() {
        return --refcount;
    }
};

extern "C" {
    void Py_DECR(PyObj* py_obj) {
        long count = py_obj->decr();
        if (count == 0) {
            delete py_obj;
        }
    }

    void Py_INCR(PyObj* py_obj) {
        py_obj->incr();
    }
}

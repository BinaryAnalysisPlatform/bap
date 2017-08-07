#ifndef LLVM_ERROR_OR_HPP
#define LLVM_ERROR_OR_HPP

#include <vector>
#include <sstream>
#include <system_error>
#include <memory>
#include <type_traits>
#include <assert.h>

// [error_or] contains either data or error message, but not both of them.
// Also, it's possible to store warnings.
//
// Basic usage.
// Creation:
//
// error_or<int> double_value(int x) {
//     if (x == 42)                // some check
//        return success(x * 2);   // check passed
//     else
//        return failure("wrong number!"); // generate error
// }
//
// Also it's possible to generate errors and warnings in other ways.
// Warnings obviously aren't errors and could complement a value.
// So, it would be better to say that error_or is either a value +
// (possible empty) warning list, or an error.
//
// typedef std::vector<int> ints;
//
// error_or< ints > make_positive(const ints &vals) {
//     error_or< ints > x = success(ints());
//     if (vals.size() == 0) {
//         x.error() << "empty vector";  // discart any value, generate error
//         return x;
//     }
//     for (std::size_t i = 0; i < vals.size(); ++i) {
//        if (vals.at(i) > 0)            // add a warning
//            x.warning() << "already positive value at " << i << " index";
//        x->push_back(std::abs(vals.at(i)));
//     }
//     return x;
// }
//
// and examine a result:
//     ...
//     error_or<ints> x = make_positive(some_vector);
//     if (!x)      // check if error exists
//         std::cerr << "some error occured: "<< x.message() << std::endl;
//     for (auto w : x.warnings())
//         std::cout << "warning! " << w << std::endl;
//     ...
//
// Also it's possible to examine error_or with || operator wihich returns a true if
// error exists (and returned failure contains a first one if there are multiply errors)
//     ...
//     error_or<int> x = foo();
//     error_or<std::string> y = bar()
//     if (failure er = x || y) {
//          er << "something really bad happened"; // add more information to error
//          return er;
//     } else {
//        return success(some_fun(*x, *y));
//     }
//     ...
//
//  && operator works as usualy:
//     ...
//     error_or<int> x = foo();
//     error_or<int> y = bar()
//     if (x && y)
//         return success(some_fun(*x, *y));
//     else
//     ...
//
// Note, that error_or has a pointer semantic.
//
// Also there is a type conversion operator, and it's permitted to use it
// only to convert error_or that contains exactly error, i.e. :
//
//  error_or<A> my_fun(error_or<B> x) {
//      if (!x) return x;
//      A a = 42;
//      return success(a)
//  }
//
//  is correct, while
//
//  error_or<A> my_fun() {
//     B b = 42;
//     return success(b)
//  }
//
//  is wrong!
//

namespace llvm {

// due to a bug in gcc library, move constructor (that has a high importance for us)
// will not available until gcc 5.0. So instead of `typedef std::stringstream info`
// we have to use such kind of wrapper
struct info {

    info() {}
    info(info &&other) { data.str(other.data.str()); }

    info & operator=(info&& other) {
        data.str(other.data.str());
        return *this;
    }

    template <typename T>
    info & operator<<(const T &t) {
        data << t;
        return *this;
    }

    std::string str() const { return data.str(); }
    void str(std::string s) { data.str(s); }

private:
    std::stringstream data;
};


template <typename T>
struct error_or {
    typedef error_or<T> self;
    typedef T value_type;
    typedef std::unique_ptr<T> pointer;

    explicit error_or(const T &t)  : payload(pointer(new T(t))) {}
    explicit error_or(T *p)  : payload(pointer(p)) {}
    explicit error_or(T &&t) : payload(pointer(new T(std::move(t)))) {}

    error_or(self &&other) : payload(std::move(other.payload))
                           , err(std::move(other.err))
                           , warns(std::move(other.warns)) {}

    bool has_error() const { return payload == nullptr; }
    std::string message() const { return err.str(); }

    T* operator ->()            { assert_me(); return payload.get(); }
    const T* operator->() const { assert_me(); return payload.get(); }
    T & operator *()            { assert_me(); return *payload;      }
    const T & operator*() const { assert_me(); return *payload;      }
    const T* get() const { return payload.get(); }
    T* get() { return payload.get(); }

    // Return false if there is an error.
    explicit operator bool() const { return !has_error(); }

    error_or<T> & operator=(error_or<T>&& other) {
        payload = pointer(std::move(other.payload));
        err = std::move(other.err);
        warns = std::move(other.warns);
        return *this;
    }

    template <typename U>
    operator error_or<U> ()  {
        assert(payload == 0); // don't use operator () for type conversions
        error_or<U> p(nullptr);
        p.fail(err.str());
        for(const info & w : warns)
            p.warning() << w.str();
        return p;
    }

    template <typename U>
    friend error_or<U> fail_with(const std::string &);

    void fail(const std::string &s) {
        payload = nullptr;
        err.str(s);
    }

    info & warning() {
        warns.push_back(info());
        return warns.back();
    }

    info & error() {
        payload = nullptr;
        return err;
    }

    self & operator<<(const std::vector<std::string> &ws) {
        for(auto w : ws)
            warning() << w;
        return *this;
    }

    std::vector<std::string> warnings() const {
        std::vector<std::string> v;
        for (const info & w : warns)
            v.push_back(w.str());
        return v;
    }

private:

    error_or(const self &) { /* deleted. use move constructor instead. */}
    error_or<T> & operator=(const error_or<T>& other) { /* deleted. use move assignment instead. */}

    void assert_me() const { assert(payload != 0); }

    template <typename X, typename Y>
    void copy_warnings(const error_or<X> &x, error_or<Y> &y) {
        for(const info & w : x.warns)
            y.warning() << w.str();
    }

private:
    pointer payload;
    info err;
    std::vector<info> warns;
}; // error_or


template <typename T>
error_or<T> fail_with(const std::string &s) {
    error_or<T> p(nullptr);
    p.err.str(s);
    return p;
}



template <typename T>
error_or<T> success(const T &t) { return error_or<T>(t); }

template <typename T>
error_or<T> success(T *p) { return error_or<T>(p); }

template <typename T>
error_or<typename std::remove_reference<T>::type> success(T &&t) {
    typedef typename std::remove_reference<T>::type value_type;
    return error_or<value_type>(std::move(t));
}


struct failure {
    typedef std::unique_ptr<std::string> str_ptr;

    failure() : msg_(nullptr) {}
    explicit failure(const std::string &s) : msg_(new std::string(s)) { }
    failure(failure &&other) : msg_(std::move(other.msg_)), info_(std::move(other.info_)) {}

    template <typename T>
    operator error_or<T> () {
        assert(msg_ != nullptr);
        return fail_with<T>(message());
    }

    explicit operator bool() const { return msg_ != nullptr; }
    std::string message() const { assert(msg_ != nullptr); return *msg_ + info_.str(); }

    template <typename T>
    failure & operator<<(const T &t) {
        info_ << t;
        return *this;
    }

private:
    str_ptr msg_;
    info info_;
};

template <typename T, typename U>
failure operator||(const error_or<T> &x, const error_or<U> &y) {
    if (!x) return failure(x.message());
    if (!y) return failure(y.message());
    return failure();
}

template <typename T>
failure operator||(const failure &x, const error_or<T> &y) {
    if (x) return failure(x.message());
    if (!y) return failure(y.message());
    return failure();
}

template <typename U, typename T, typename Unary_function>
error_or<U> map_value(const error_or<T> &v, Unary_function op) {
    if (!v)
        return failure(v.message());
    return success(op(*v));
}

} // namespace llvm

#endif // LLVM_ERROR_OR_HPP

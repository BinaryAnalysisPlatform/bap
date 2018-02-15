#ifndef LLVM_LOADER_UTILS_HPP
#define LLVM_LOADER_UTILS_HPP

#include <sstream>
#include <iomanip>

#include <llvm/ADT/Triple.h>

#include "llvm_error_or.hpp"

namespace loader {

using namespace llvm;

bool str_mem(const std::string &s, char x) {
    return (s.find(x) != std::string::npos);
}

bool str_mem_any(const std::string &s, const std::string &p) {
    return std::any_of(s.begin(), s.end(), [=](const char &x)
                       {return str_mem(p,x);});
}

std::string smart_quoted(const std::string &s) {
    std::string p = "() /\\";
    if (str_mem_any(s, p) || s == "") return "\"" + s + "\"";
    else return s;
}


// ogre doc
// makes it easy to brew an ogre document.
// It's recomended to use it in the folowing ways:
// ...
// doc.entry("entry-name") << 42 << false;
// ...
// This return a doc with (entry-name 42 false) content.
// All strings that contain slashes, parentheses or spaces
// will be quoted. Entry name itself doesn't quoted
// Also possible to add a raw entry, i.e data will be
// added as it is:
// ...
// doc.raw_entry("(entry-name 42 false)");
// ...
struct ogre_doc {

    explicit ogre_doc() : s_(info()), closed_(true) {
        *s_ << std::boolalpha << std::hex << std::showbase;
    }

    void fail(const std::string &m) { s_.fail(m); }

    ogre_doc & entry(const std::string &name) {
        if (s_) {
            close();
            *s_ << "(" << name;
            closed_ = false;
        }
        return *this;
    }

    template <typename T>
    friend ogre_doc & operator<<(ogre_doc &d, const T &t) {
        if (d.s_) *d.s_ << " " << t;
        return d;
    }

    friend ogre_doc & operator<<(ogre_doc &d, const std::string &t) {
        if (d.s_) *d.s_ << " " << smart_quoted(t);
        return d;
    }

    friend ogre_doc & operator<<(ogre_doc &d, const char *t) {
        return d << std::string(t);
    }

    void raw_entry(const std::string &data) {
        close();
        if (s_) *s_ << data;
    }

    error_or<std::string> str() {
        close();
        if (s_) return success(s_->str());
        else    return failure(s_.message());
    }

private:
    void close() {
        if (s_ && !closed_) { *s_ << ")"; closed_ = true; }
    }

private:
    error_or<info> s_;
    bool closed_;
};

} // namespace loader

#endif // LLVM_LOADER_UTILS_HPP

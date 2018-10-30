#ifndef LLVM_LOADER_UTILS_HPP
#define LLVM_LOADER_UTILS_HPP

#include <sstream>
#include <iomanip>

#include <llvm/ADT/Triple.h>

#include "llvm_error_or.hpp"

namespace loader {

using namespace llvm;

std::string escape(const std::string &src) {
    std::stringstream dst;
    for (auto it = src.begin(); it != src.end (); ++it) {
        if (isalpha(*it) || isdigit(*it))
            dst << *it;
        else
            dst << "\\x" << std::hex << int(*it) ;
    }
    return dst.str();
}

// ogre doc
// makes it easy to brew an ogre document.
// It's recomended to use it in the folowing ways:
// ...
// doc.entry("entry-name") << 42 << false;
// ...
// This return a doc with (entry-name 42 false) content.
// All strings are quoted. Also, all characters except
// digits and letters are replaced by their's ascii codes.
// Entry name itself doesn't quoted.
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
        if (d.s_) *d.s_ << " " << "\"" << escape(t) << "\"";
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

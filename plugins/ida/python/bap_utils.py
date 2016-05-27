"""Commonly used utilities for BAP IDAPython plugins to use."""

import idaapi


def sexp2list(s):
    """Convert S-Expression to List"""
    sexp = [[]]
    word = ''
    for c in s:
        if c == '(':
            sexp.append([])
        elif c == ')':
            if word:
                sexp[-1].append(word)
                word = ''
            temp = sexp.pop()
            sexp[-1].append(temp)
        elif c == ' ':
            if word:
                sexp[-1].append(word)
            word = ''
        else:
            word += c
    return sexp[0]


def list2sexp(l):
    """Convert List to S-Expression"""
    if isinstance(l, str):
        return l
    return '(' + ' '.join(list2sexp(e) for e in l) + ')'


def add_to_comment_string(comm, key, value):
    """Add key:value to comm string"""
    if '(BAP ' in comm:
        start_loc = comm.index('(BAP ')
        bracket_count = 0
        for i in range(start_loc, len(comm)):
            if comm[i] == '(':
                bracket_count += 1
            elif comm[i] == ')':
                bracket_count -= 1
                if bracket_count == 0:
                    end_loc = i + 1
                    BAP_dict = comm[start_loc:end_loc]
                    break
        else:
            # Invalid bracketing.
            # Someone messed up the dict.
            # Correct by inserting enough close brackets.
            end_loc = len(comm)
            BAP_dict = comm[start_loc:end_loc] + (')' * bracket_count)
    else:
        start_loc = len(comm)
        end_loc = len(comm)
        BAP_dict = '(BAP )'

    kv = ['BAP', [key, value]]
    for e in sexp2list(BAP_dict[5:-1]):  # Remove outermost '(BAP', ')'
        if isinstance(e, list) and len(e) == 2:  # It is of the '(k v)' type
            if e[0] != key:  # Don't append if same as required key
                kv.append(e)
        else:
            kv.append(e)

    return comm[:start_loc] + list2sexp(kv) + comm[end_loc:]


class DoNothing(idaapi.plugin_t):
    """
    Do Nothing.

    This plugin does absolutely nothing. It is created for the sole purpose of
    being able to keep multiple non-plugin Python files which may then be used
    as utilities by other plugins.

    Usage:
        import bap_utils

        class DoNothing<SomeUniqueIdentifier>(bap_utils.DoNothing):
            pass

        def PLUGIN_ENTRY():
            return DoNothing<SomeUniqueIdentifier>()
    """

    flags = idaapi.PLUGIN_UNL
    comment = "Does Nothing"
    help = "Does Nothing"
    wanted_name = "Do Nothing"
    wanted_hotkey = ""

    def init(self):
        """Skip plugin."""
        return idaapi.PLUGIN_OK

    def term(self):
        """Do nothing."""
        pass

    def run(self, arg):
        """Do nothing."""
        pass


def PLUGIN_ENTRY():
    """Do not count this file as a plugin."""
    return DoNothing()

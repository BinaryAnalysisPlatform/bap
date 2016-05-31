"""Commonly used utilities for BAP IDAPython plugins to use."""

import idaapi


def sexp2list(s):
    """Convert S-Expression to List."""
    sexp = [[]]
    word = ''
    in_str = False
    for c in s:
        if c == '(' and not in_str:
            sexp.append([])
        elif c == ')' and not in_str:
            if word:
                sexp[-1].append(word)
                word = ''
            temp = sexp.pop()
            sexp[-1].append(temp)
        elif c in (' ', '\n', '\t') and not in_str:
            if word:
                sexp[-1].append(word)
            word = ''
        elif c == '\"':
            in_str = not in_str
        else:
            word += c
    return sexp[0]


def list2sexp(l):
    """Convert List to S-Expression."""
    if isinstance(l, str):
        for special_char in (' ', '\n', '\t', '(', ')', '\"'):
            if special_char in l:
                return '\"' + l + '\"'
        return l
    return '(' + ' '.join(list2sexp(e) for e in l) + ')'


def get_bap_comment(comm):
    """
    Get '(BAP )' style comment from given string.

    Returns tuple (BAP_dict, start_loc, end_loc)
        BAP_dict: The '(BAP )' style comment
        start_loc: comm[:start_loc] was before the BAP comment
        end_loc: comm[end_loc:] was after the BAP comment
    """
    if '(BAP ' in comm:
        start_loc = comm.index('(BAP ')
        bracket_count = 0
        in_str = False
        for i in range(start_loc, len(comm)):
            if comm[i] == '(' and not in_str:
                bracket_count += 1
            elif comm[i] == ')' and not in_str:
                bracket_count -= 1
                if bracket_count == 0:
                    end_loc = i + 1
                    BAP_dict = comm[start_loc:end_loc]
                    break
            elif comm[i] == '\"':
                in_str = not in_str
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

    return (BAP_dict, start_loc, end_loc)


def get_bap_list(BAP_dict):
    """Return a list containing all the values in the BAP comment."""
    outer_removed = BAP_dict[5:-1]  # Remove outermost '(BAP', ')'
    return sexp2list(outer_removed)


def add_to_comment_string(comm, key, value):
    """Add key:value to comm string."""
    BAP_dict, start_loc, end_loc = get_bap_comment(comm)

    kv = ['BAP', [key, value]]
    for e in get_bap_list(BAP_dict):
        if isinstance(e, list) and len(e) == 2:  # It is of the '(k v)' type
            if e[0] != key:  # Don't append if same as required key
                kv.append(e)
        else:
            kv.append(e)

    return comm[:start_loc] + list2sexp(kv) + comm[end_loc:]


def add_to_comment(ea, key, value):
    """Add key:value to comm string at EA."""
    old_comm = idaapi.get_cmt(ea, 0)
    if old_comm is None:
        old_comm = ''
    new_comm = add_to_comment_string(old_comm, key, value)
    idaapi.set_cmt(ea, new_comm, 0)


def cfunc_from_ea(ea):
    """Get cfuncptr_t from EA."""
    func = idaapi.get_func(ea)
    if func is None:
        return None
    cfunc = idaapi.decompile(func)
    return cfunc


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

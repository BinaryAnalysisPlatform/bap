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


def dump_loader_info(output_filename):
    """Dump information for BAP's loader into output_filename."""
    from idautils import Segments
    import idc

    idaapi.autoWait()

    with open(output_filename, 'w+') as out:
        info = idaapi.get_inf_structure()
        size = "r32" if info.is_32bit else "r64"
        out.write("(%s %s (" % (info.get_proc_name()[1], size))
        for seg in Segments():
            out.write("\n(%s %s %d (0x%X %d))" % (
                idaapi.get_segm_name(seg),
                "code" if idaapi.segtype(seg) == idaapi.SEG_CODE else "data",
                idaapi.get_fileregion_offset(seg),
                seg, idaapi.getseg(seg).size()))
        out.write("))\n")


def dump_symbol_info(output_filename):
    """Dump information for BAP's symbolizer into output_filename."""
    from idautils import Segments, Functions
    from idc import (
        SegStart, SegEnd, GetFunctionAttr,
        FUNCATTR_START, FUNCATTR_END
    )

    try:
        from idaapi import get_func_name2 as get_func_name
        # Since get_func_name is deprecated (at least from IDA 6.9)
    except ImportError:
        pass
        # Older versions of IDA don't have get_func_name2
        # so we just use the older name get_func_name

    def func_name_propagate_thunk(ea):
        current_name = get_func_name(ea)
        if current_name[0].isalpha():
            return current_name
        func = idaapi.get_func(ea)
        temp_ptr = idaapi.ea_pointer()
        ea_new = idaapi.BADADDR
        if func.flags & idaapi.FUNC_THUNK == idaapi.FUNC_THUNK:
            ea_new = idaapi.calc_thunk_func_target(func, temp_ptr.cast())
        if ea_new != idaapi.BADADDR:
            ea = ea_new
        propagated_name = get_func_name(ea)
        if len(current_name) > len(propagated_name) > 0:
            return propagated_name
        else:
            return current_name
            # Fallback to non-propagated name for weird times that IDA gives
            #     a 0 length name, or finds a longer import name

    idaapi.autoWait()

    with open(output_filename, 'w+') as out:
        for ea in Segments():
            fs = Functions(SegStart(ea), SegEnd(ea))
            for f in fs:
                out.write('(%s 0x%x 0x%x)\n' % (
                    func_name_propagate_thunk(f),
                    GetFunctionAttr(f, FUNCATTR_START),
                    GetFunctionAttr(f, FUNCATTR_END)))


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

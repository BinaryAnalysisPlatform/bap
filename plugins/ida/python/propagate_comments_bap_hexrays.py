"""Hex-Rays Plugin to propagate comments to Pseudocode View."""

from simpleline_modifier_hexrays import SimpleLine_Modifier_Hexrays
import bap_utils


class BAP_Comment_Pseudocode(SimpleLine_Modifier_Hexrays):
    """Propagate comments from Text/Graph view to Pseudocode view."""

    @classmethod
    def _simpleline_modify(cls, cfunc, sl):
        sl_dict = {}

        ea_set = set(cls.get_ea_list(cfunc, sl))

        for ea in ea_set:
            ea_comm = GetCommentEx(ea, repeatable=0)
            if ea_comm is None:
                continue
            ea_BAP_dict, _, _ = bap_utils.get_bap_comment(ea_comm)
            for e in bap_utils.get_bap_list(ea_BAP_dict):
                if isinstance(e, list) and len(e) >= 2:  # i.e. '(k v)' type
                    val_list = sl_dict.get(e[0], [])
                    if e[1:] not in val_list:
                        val_list.append(e[1:])
                    sl_dict[e[0]] = val_list

        if len(sl_dict) > 0:
            BAP_dict = ['BAP']
            for k, v in sl_dict.items():
                BAP_dict += [[k] + v]
            BAP_comment = bap_utils.list2sexp(BAP_dict)

            t = idaapi.treeloc_t()
            t.ea = max(ea_set)
            t.itp = idaapi.ITP_BLOCK1  # Block comment before the statement
            cfunc.set_user_cmt(t, BAP_comment)
            cfunc.save_user_cmts()

            # TODO: Currently, the plugin requires you to refresh to be able
            #       to see the added BAP comments. Might be nice if it
            #       internally did that, thought it might be hard since
            #       the plugin installs itself as a refresh text callback

    comment = "BAP Comment on Pseudocode"
    help = "BAP Comment on Pseudocode"
    wanted_name = "BAP Comment on Pseudocode"


def PLUGIN_ENTRY():
    """Install BAP_Comment_Pseudocode upon entry."""
    return BAP_Comment_Pseudocode()

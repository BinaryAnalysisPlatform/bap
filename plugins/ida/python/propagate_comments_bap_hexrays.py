"""Hex-Rays Plugin to propagate comments to Pseudocode View."""

from simpleline_modifier_hexrays import SimpleLine_Modifier_Hexrays
import bap_utils


class BAP_Comment_Pseudocode(SimpleLine_Modifier_Hexrays):
    """Propagate comments from Text/Graph view to Pseudocode view."""

    @classmethod
    def _simpleline_modify(cls, cfunc, sl):
        sl_dict = {}

        for ea in set(cls.get_ea_list(cfunc, sl)):
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
            sl.line += '\x01\x0c // \x01\x0c'  # start comment coloring
            sl.line += bap_utils.list2sexp(BAP_dict)
            sl.line += '\x02\x0c\x02\x0c'  # stop comment coloring

    comment = "BAP Comment on Pseudocode"
    help = "BAP Comment on Pseudocode"
    wanted_name = "BAP Comment on Pseudocode"


def PLUGIN_ENTRY():
    """Install BAP_Comment_Pseudocode upon entry."""
    return BAP_Comment_Pseudocode()

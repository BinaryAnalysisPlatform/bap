"""
Hex-Rays Plugin to propagate taint information to Pseudocode View.

Requires BAP_Taint plugin, and installs callbacks into it.
"""

bap_color = {
    'black':   0x000000,
    'red':     0xCCCCFF,
    'green':   0x99FF99,
    'yellow':  0xC2FFFF,
    'blue':    0xFFB2B2,
    'magenta': 0xFFB2FF,
    'cyan':    0xFFFFB2,
    'white':   0xFFFFFF,
    'gray':    0xEAEAEA,
}

from simpleline_modifier_hexrays import SimpleLine_Modifier_Hexrays
import bap_utils


class BAP_Taint_Pseudocode(SimpleLine_Modifier_Hexrays):
    """Propagate taint information from Text/Graph view to Pseudocode view."""

    comment = "BAP Taint Plugin for Pseudocode View"
    help = "BAP Taint Plugin for Pseudocode View"
    wanted_name = "BAP Taint Pseudocode"

    @classmethod
    def _simpleline_modify(cls, cfunc, sl):
        cls._color_line(sl, bap_color['gray'])
        # Ready to be painted over by other colors
        for ea in cls.get_ea_list(cfunc, sl):
            new_color = cls._get_new_color(sl.bgcolor, ea)
            cls._color_line(sl, new_color)

    @staticmethod
    def _color_line(sl, color):
        sl.bgcolor = color

    @staticmethod
    def _get_new_color(current_color, ea):
        coloring_order = [
            bap_color[c] for c in [
                'gray',
                'white',
                'red',
                'yellow',
            ]
        ]

        BGR_MASK = 0xffffff

        ea_color = idaapi.get_item_color(ea)

        if ea_color & BGR_MASK not in coloring_order:
        # Since BAP didn't color it, we can't infer anything
            return current_color

        assert(current_color & BGR_MASK in coloring_order)

        ea_idx = coloring_order.index(ea_color & BGR_MASK)
        current_idx = coloring_order.index(current_color & BGR_MASK)

        if ea_idx >= current_idx:
            return ea_color
        else:
            return current_color

    def init(self):
        """Initialize Plugin."""
        try:
            if idaapi.init_hexrays_plugin():

                def autocolorize_callback(data):
                    ea = data['ea']
                    cfunc = bap_utils.cfunc_from_ea(ea)
                    if cfunc is None:
                        return
                    self.run_over_cfunc(cfunc)

                idaapi.load_and_run_plugin('bap_propagate_taint.py', 0)
                BAP_Taint.install_callback(autocolorize_callback)

                print ("Finished installing callbacks for Taint Analysis" +
                       " in Hex-Rays")

            else:
                return idaapi.PLUGIN_SKIP

        except AttributeError:
            print "init_hexrays_plugin() not found. Skipping Hex-Rays plugin."

        return SimpleLine_Modifier_Hexrays.init(self)  # Call superclass init()


def PLUGIN_ENTRY():
    """Install BAP_Taint_Pseudocode upon entry."""
    return BAP_Taint_Pseudocode()

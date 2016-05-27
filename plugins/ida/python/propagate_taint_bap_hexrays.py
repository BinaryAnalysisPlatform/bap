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


class BAP_Taint_Pseudocode(idaapi.plugin_t):
    """Propagate taint information from Text/Graph view to Pseudocode view."""

    def _autocolorize_function(self, cfunc):
        simplelinevec = cfunc.get_pseudocode()

        def ea_from_addr_tag(addr_tag):
            return cfunc.treeitems.at(addr_tag).ea

        for simpleline in simplelinevec:
            self._color_line(simpleline, bap_color['gray'])
                # Ready to be painted over by other colors
            self._autocolorize_line(simpleline, ea_from_addr_tag)

    def _color_line(self, simpleline, color):
        simpleline.bgcolor = color

    def _get_new_color(self, current_color, ea):
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

    def _autocolorize_line(self, simpleline, ea_from_tag):
        anchor = idaapi.ctree_anchor_t()
        line = simpleline.line[:]  # Copy

        def is_addr_code(l):
            return (l[0] == idaapi.COLOR_ON and
                    l[1] == chr(idaapi.COLOR_ADDR))

        while len(line) > 0:
            skipcode_index = idaapi.tag_skipcode(line)
            if skipcode_index == 0:  # No code found
                line = line[1:]  # Skip one character ahead
            else:
                if is_addr_code(line):
                    addr_tag = int(line[2:skipcode_index], 16)
                    anchor.value = addr_tag
                    if anchor.is_citem_anchor():
                        line_ea = ea_from_tag(addr_tag)
                        if line_ea != idaapi.BADADDR:
                            new_color = self._get_new_color(simpleline.bgcolor,
                                                            line_ea)
                            self._color_line(simpleline, new_color)
                line = line[skipcode_index:]  # Skip the colorcodes

    flags = idaapi.PLUGIN_FIX
    comment = "BAP Taint Plugin for Pseudocode View"
    help = "BAP Taint Plugin for Pseudocode View"
    wanted_name = "BAP Taint Pseudocode"
    wanted_hotkey = ""

    def init(self):
        """Initialize Plugin."""
        try:
            if idaapi.init_hexrays_plugin():
                def hexrays_event_callback(event, *args):
                    if event == idaapi.hxe_text_ready:
                        vu, = args
                        self._autocolorize_function(vu.cfunc)
                    return 0

                idaapi.install_hexrays_callback(hexrays_event_callback)

                def cfunc_from_ea(ea):
                    func = idaapi.get_func(ea)
                    if func is None:
                        return None
                    cfunc = idaapi.decompile(func)
                    return cfunc

                def autocolorize_callback(data):
                    ea = data['ea']
                    cfunc = cfunc_from_ea(ea)
                    if cfunc is None:
                        return
                    self._autocolorize_function(cfunc)

                idaapi.load_and_run_plugin('bap_propagate_taint.py', 0)
                BAP_Taint.install_callback(autocolorize_callback)

                print ("Finished installing callbacks for Taint Analysis" +
                       " in Hex-Rays")

            else:
                print "Hex-Rays not loaded"

        except AttributeError:
            print "init_hexrays_plugin() not found. Skipping Hex-Rays plugin."

        return idaapi.PLUGIN_KEEP

    def term(self):
        """Terminate Plugin."""
        try:
            idaapi.term_hexrays_plugin()
        except AttributeError:
            pass

    def run(self, arg):
        """
        Run Plugin.

        Ignored since callbacks are installed.
        """
        pass


def PLUGIN_ENTRY():
    """Install BAP_Taint_Pseudocode upon entry."""
    return BAP_Taint_Pseudocode()

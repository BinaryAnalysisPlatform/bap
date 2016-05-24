bap_color = {
    'black'  : 0x000000,
    'red'    : 0xCCCCFF,
    'green'  : 0x99FF99,
    'yellow' : 0xC2FFFF,
    'blue'   : 0xFFB2B2,
    'magenta': 0xFFB2FF,
    'cyan'   : 0xFFFFB2,
    'white'  : 0xFFFFFF,
    'gray'   : 0xEAEAEA,
}


class BAP_Taint_Pseudocode(idaapi.plugin_t):

    def autocolorize_function(self, cfunc):
        simplelinevec = cfunc.get_pseudocode()

        def ea_from_addr_tag(addr_tag):
            return cfunc.treeitems.at(addr_tag).ea

        for simpleline in simplelinevec:
            self.color_line(simpleline, bap_color['gray'])
                # Ready to be painted over by other colors
            self.autocolorize_line(simpleline, ea_from_addr_tag)

    def color_line(self, simpleline, color):
        simpleline.bgcolor = color

    def get_new_color(self, current_color, ea):
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

        assert(ea_color & BGR_MASK in coloring_order)
        assert(current_color & BGR_MASK in coloring_order)

        ea_idx = coloring_order.index(ea_color & BGR_MASK)
        current_idx = coloring_order.index(current_color & BGR_MASK)

        if ea_idx >= current_idx:
            return ea_color
        else:
            return current_color

    def autocolorize_line(self, simpleline, ea_from_tag):
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
                            new_color = self.get_new_color(simpleline.bgcolor,
                                                           line_ea)
                            self.color_line(simpleline, new_color)
                line = line[skipcode_index:]  # Skip the colorcodes

    flags = idaapi.PLUGIN_FIX
    comment = "BAP Taint Plugin for Pseudocode View"
    help = "BAP Taint Plugin for Pseudocode View"
    wanted_name = "BAP Taint Pseudocode"
    wanted_hotkey = ""

    def init(self):
        if idaapi.init_hexrays_plugin():
            def hexrays_event_callback(event, *args):
                if event == idaapi.hxe_text_ready:
                    vu, = args
                    self.autocolorize_function(vu.cfunc)
                return 0

            idaapi.install_hexrays_callback(hexrays_event_callback)

            def cfunc_from_ea(ea):
                func = idaapi.get_func(ea)
                cfunc = idaapi.decompile(func)
                return cfunc

            def autocolorize_callback(data):
                ea = data['ea']
                self.autocolorize_function(cfunc_from_ea(ea))

            idaapi.load_and_run_plugin('bap_propagate_taint', 0)
            BAP_Taint.install_callback(autocolorize_callback)

            print "Finished installing callbacks for Taint Analysis in Hex-Rays"

        else:

            print "Hex-Rays not loaded"

    def term(self):
        idaapi.term_hexrays_plugin()

    def run(self, arg):
        pass


def PLUGIN_ENTRY():
    return BAP_Taint_Pseudocode()

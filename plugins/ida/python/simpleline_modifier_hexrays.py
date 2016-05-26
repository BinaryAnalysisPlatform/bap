"""
Abstract Base Plugin Class to modify simplelines in Pseudocode View.

This plugin should be subclassed and the following members should be
implemented before being usable:
    - simpleline_modify(cls, cfunc, sl)
        - Accepts a simpleline_t (sl) and modifies it
        - cfunc and class cls are provided for use if necessary
    - comment
        - string to describe your plugin
    - help
        - string for any help information
    - wanted_name
        - string for what you want your plugin to be named

Methods that might be useful while implementing above methods:
    - get_ea_list(cls, cfunc, sl)

Note: You will need to add a PLUGIN_ENTRY() function, to your plugin code,
that returns an object of your plugin, which uses this Class as a super class.
"""


class SimpleLine_Modifier_Hexrays(idaapi.plugin_t):
    """Modifies each simpleline_t in Pseudocode view."""

    @classmethod
    def _simpleline_modify(cls, cfunc, sl):
        raise NotImplementedError("Please implement this method")

    @classmethod
    def get_ea_list(cls, cfunc, sl):
        """Get a list of EAs that are in a simpleline_t."""
        def ea_from_addr_tag(addr_tag):
            return cfunc.treeitems.at(addr_tag).ea

        def is_addr_code(s):
            return (s[0] == idaapi.COLOR_ON and
                    s[1] == chr(idaapi.COLOR_ADDR))

        anchor = idaapi.ctree_anchor_t()
        line = simpleline.line[:]  # Copy
        ea_list = []

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
                            ea_list.append(line_ea)
                line = line[skipcode_index:]  # Skip the colorcodes

        return ea_list

    @classmethod
    def run_over_cfunc(cls, cfunc):
        """Run the plugin over the given cfunc."""
        simplelinevec = cfunc.get_pseudocode()

        for simpleline in simplelinevec:
            cls._simpleline_modify(cfunc, simpleline)

    flags = idaapi.PLUGIN_FIX
    wanted_hotkey = ""

    def init(self):
        """
        Ensure plugin's line modification function is called whenever needed.

        If Hex-Rays is not installed, or is not initialized yet, then plugin
        will not load. To ensure that the plugin loads after Hex-Rays, please
        name your plugin's .py file with a name that starts lexicographically
        after "hexx86f"
        """
        try:
            if idaapi.init_hexrays_plugin():
                def hexrays_event_callback(event, *args):
                    if event == idaapi.hxe_text_ready:
                        vu, = args
                        self._run_over_cfunc(vu.cfunc)
                    return 0

                idaapi.install_hexrays_callback(hexrays_event_callback)

            else:
                print "Hex-Rays not loaded"

        except AttributeError:
            print "init_hexrays_plugin() not found. Skipping Hex-Rays plugin."

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

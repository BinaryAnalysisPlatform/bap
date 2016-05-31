"""
IDA Python Plugin to use BAP to propagate taint information.

Allows user to select any arbitrary line in Graph/Text view in IDA,
and be able to taint that line and propagate taint information.

Keybindings:
    Shift-A      : Equivalent to `--taint-reg` in BAP
    Ctrl-Shift-A : Equivalent to `--taint-ptr` in BAP

Color Scheme:
    "Nasty" Yellow : Taint source
    "Affected" Red : Lines that got tainted
    "Ignored" Gray : Lines that were not visited by propagate-taint
    "Normal" White : Lines that were visited, but didn't get tainted
"""
import idautils


class BAP_Taint(idaapi.plugin_t):
    """
    Plugin to use BAP to propagate taint information.

    Also supports installation of callbacks using install_callback()
    """

    _callbacks = {
        'ptr': [],
        'reg': []
    }

    @classmethod
    def _do_callbacks(cls, ptr_or_reg):
        assert(ptr_or_reg == 'reg' or ptr_or_reg == 'ptr')
        data = {
            'ea': idc.ScreenEA(),
            'ptr_or_reg': ptr_or_reg
        }
        for callback in cls._callbacks[ptr_or_reg]:
            callback(data)

    def _taint_and_color(self, ptr_or_reg):
        import tempfile

        args = {
            'input_file_path': idc.GetInputFilePath(),
            'taint_location': idc.ScreenEA(),
            'ida_script_location': tempfile.mkstemp(suffix='.py',
                                                    prefix='ida-bap-')[1],
            'symbol_file_location': tempfile.mkstemp(suffix='.sym',
                                                     prefix='ida-bap-')[1],
            'ptr_or_reg': ptr_or_reg
        }

        idc.Message('-------- STARTING TAINT ANALYSIS --------------\n')
        idc.SetStatus(IDA_STATUS_WAITING)

        idaapi.refresh_idaview_anyway()

        dump_symbol_info(args['symbol_file_location'])

        idc.Exec(
            "\
            $(bindir)/bap \"{input_file_path}\" \
            --read-symbols-from={symbol_file_location} --symbolizer=file \
            --taint-{ptr_or_reg}=0x{taint_location:X} \
            --taint \
            --propagate-taint \
            --map-terms-with='((true) (color gray))' \
            --map-terms-with='((is-visited) (color white))' \
            --map-terms-with='((has-taints) (color red))' \
            --map-terms-with='((taints) (color yellow))' \
            --map-terms \
            --emit-ida-script-attr=color \
            --emit-ida-script-file={ida_script_location} \
            --emit-ida-script \
            ".format(**args)
        )

        idc.Message('-------- DONE WITH TAINT ANALYSIS -------------\n\n')
        idc.SetStatus(IDA_STATUS_READY)

        idaapi.IDAPython_ExecScript(args['ida_script_location'], globals())

        idc.Exec("rm -f \"{ida_script_location}\" \"{symbol_file_location}\""
                 .format(**args))  # Cleanup

        idc.Refresh()  # Force the color information to show up

        self._do_callbacks(ptr_or_reg)

    def _taint_reg_and_color(self):
        self._taint_and_color('reg')

    def _taint_ptr_and_color(self):
        self._taint_and_color('ptr')

    def _add_hotkey(self, hotkey, func):
        hotkey_ctx = idaapi.add_hotkey(hotkey, func)
        if hotkey_ctx is None:
            print("Failed to register {} for {}".format(hotkey, func))
        else:
            print("Registered {} for {}".format(hotkey, func))

    flags = idaapi.PLUGIN_FIX
    comment = "BAP Taint Plugin"
    help = "BAP Taint Plugin"
    wanted_name = "BAP Taint Plugin"
    wanted_hotkey = ""

    def init(self):
        """Initialize Plugin."""
        self._add_hotkey("Shift-A", self._taint_reg_and_color)
        self._add_hotkey("Ctrl-Shift-A", self._taint_ptr_and_color)
        return idaapi.PLUGIN_KEEP

    def term(self):
        """Terminate Plugin."""
        pass

    def run(self, arg):
        """
        Run Plugin.

        Ignored since keybindings are installed.
        """
        pass

    @classmethod
    def install_callback(cls, callback_fn, ptr_or_reg=None):
        """
        Install callback to be run when the user calls for taint propagation.

        Callback must take a dict and must return nothing.

        Dict is guaranteed to get the following keys:
            'ea': The value of EA at point where user propagated taint from.
            'ptr_or_reg': Either 'ptr' or 'reg' depending on user selection.
        """
        if ptr_or_reg is None:
            cls.install_callback(callback_fn, 'ptr')
            cls.install_callback(callback_fn, 'reg')
        elif ptr_or_reg == 'ptr' or ptr_or_reg == 'reg':
            cls._callbacks[ptr_or_reg].append(callback_fn)
        else:
            print "Invalid ptr_or_reg value passed {}".format(repr(ptr_or_reg))


def PLUGIN_ENTRY():
    """Install BAP_Taint upon entry."""
    return BAP_Taint()

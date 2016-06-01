"""
IDA Python Plugin to get BIR attributes from an arbitrary BAP execution.

Allows user to run any BAP plugins and get information from BIR attributes,
into comments in IDA. Use the selected line's address in the command using
"{screen_ea}".

Keybindings:
    Shift-S  : Open up a window to accept arbitrary BAP commands,
                and select arbitrary BIR attributes to output to IDA comments

Comment Format:
    Comments in the Text/Graph Views are added using a key-value storage
    with the format (BAP (k1 v1) (k2 v2) ...)
"""
import idautils


class BAP_BIR_Attr(idaapi.plugin_t):
    """
    Plugin to get BIR attributes from arbitrary BAP executions.

    Also supports installation of callbacks using install_callback()
    """

    _callbacks = []

    @classmethod
    def _do_callbacks(cls):
        data = {
            'ea': idc.ScreenEA(),
        }
        for callback in cls._callbacks:
            callback(data)

    @classmethod
    def run_bap(cls):
        import tempfile
        from bap_utils import run_bap_with

        args = {
            'screen_ea': "0x{:X}".format(idc.ScreenEA()),
            'ida_script_location': tempfile.mkstemp(suffix='.py',
                                                    prefix='ida-bap-')[1],
            'args_from_user': idaapi.askstr(0, '', 'Args to pass to BAP'),
            'bir_attr': idaapi.askstr(0, 'comment', 'BIR Attributes')
        }

        if args['args_from_user'] is None:
            args['args_from_user'] = ''

        if args['bir_attr'] is not None:
            args['args_from_user'] += "\
            --emit-ida-script-attr={bir_attr} \
            --emit-ida-script-file={ida_script_location} \
            --emit-ida-script \
            ".format(**args)

        idc.SetStatus(IDA_STATUS_WAITING)
        idaapi.refresh_idaview_anyway()

        run_bap_with(args['args_from_user'].format(args))

        idc.SetStatus(IDA_STATUS_READY)

        idaapi.IDAPython_ExecScript(args['ida_script_location'], globals())

        idc.Exec("rm -f \"{ida_script_location}\"".format(**args))  # Cleanup

        idc.Refresh()  # Force the updated information to show up

        cls._do_callbacks()

    flags = idaapi.PLUGIN_FIX
    comment = "BAP BIR Attr Plugin"
    help = "BAP BIR Attr Plugin"
    wanted_name = "BAP BIR Attr Plugin"
    wanted_hotkey = ""

    def init(self):
        """Initialize Plugin."""
        from bap_utils import add_hotkey
        add_hotkey("Shift-S", self.run_bap)
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
    def install_callback(cls, callback_fn):
        """
        Install callback to be run when the user calls for BAP execution.

        Callback must take a dict and must return nothing.

        Dict is guaranteed to get the following keys:
            'ea': The value of EA at point where user propagated taint from.
        """
        cls._callbacks[ptr_or_reg].append(callback_fn)


def PLUGIN_ENTRY():
    """Install BAP_BIR_Attr upon entry."""
    return BAP_BIR_Attr()

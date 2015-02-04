(function() {
    function getQueryParam(name, queryString) {
        var match = RegExp(name + '=([^&]*)').exec(queryString || location.search);
        return match && decodeURIComponent(match[1]);
    }

    function hasOption(opt) {
        var s = window.location.search;
        var re = new RegExp('(?:^|[&?])' + opt + '(?:[=]([^&]*))?(?:$|[&])', 'i');
        var m = re.exec(s);

        return m ? (m[1] === undefined ? true : m[1]) : false;
    }

    var scriptTags = document.getElementsByTagName('script'),
        defaultTheme = 'neptune',
        defaultRtl = false,
        i = scriptTags.length,
        requires = [
            'Ext.toolbar.Toolbar',
            'Ext.form.field.ComboBox',
            'Ext.form.FieldContainer',
            'Ext.form.field.Radio'

        ],
        defaultQueryString, src, theme, rtl;

    while (i--) {
        src = scriptTags[i].src;
        if (src.indexOf('include-ext.js') !== -1) {
            defaultQueryString = src.split('?')[1];
            if (defaultQueryString) {
                defaultTheme = getQueryParam('theme', defaultQueryString) || defaultTheme;
                defaultRtl = getQueryParam('rtl', defaultQueryString) || defaultRtl;
            }
            break;
        }
    }

    Ext.themeName = theme = getQueryParam('theme') || defaultTheme;
    
    rtl = getQueryParam('rtl') || defaultRtl;

    if (rtl.toString() === 'true') {
        requires.push('Ext.rtl.*');
        Ext.define('Ext.GlobalRtlComponent', {
            override: 'Ext.AbstractComponent',
            rtl: true
        });
    }

    Ext.require(requires);

    Ext.onReady(function() {
        Ext.getBody().addCls(Ext.baseCSSPrefix + 'theme-' + Ext.themeName);

        if (Ext.isIE6 && theme === 'neptune') {
            Ext.Msg.show({
                title: 'Browser Not Supported',
                msg: 'The Neptune theme is not supported in IE6.',
                buttons: Ext.Msg.OK,
                icon: Ext.Msg.WARNING
            });
        }
        
        if (hasOption('nocss3')) {
            Ext.supports.CSS3BorderRadius = false;
            Ext.getBody().addCls('x-nbr x-nlg');
        }
        function setParam(param) {
            var queryString = Ext.Object.toQueryString(
                Ext.apply(Ext.Object.fromQueryString(location.search), param)
            );
            location.search = queryString;
        }

        function removeParam(paramName) {
            var params = Ext.Object.fromQueryString(location.search);

            delete params[paramName];

            location.search = Ext.Object.toQueryString(params);
        }

    });
})();

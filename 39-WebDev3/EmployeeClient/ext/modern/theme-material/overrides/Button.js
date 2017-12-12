Ext.define('Ext.theme.material.Button', {
    override: 'Ext.Button',

    config: {
        ripple: {
            delegate: '.' + Ext.baseCSSPrefix + 'inner-el'
        }
    },

    materialIconRe: /^md-icon[-|_](.*)/,

    applyIconCls: function (classList) {
        if (classList) {
            classList = Ext.dom.Element.splitCls(classList);

            var len = classList.length,
                i, cls, materialMatch;

            for (i = 0; i < len; i++) {
                cls = classList[i];
                materialMatch = cls && cls.match(this.materialIconRe);
                if (materialMatch && materialMatch.length > 1) {
                    classList.unshift('md-icon');
                    break;
                }
            }

            return classList.join(' ');
        }

        return classList;
    }
});

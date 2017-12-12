Ext.require([
    'Ext.viewport.Viewport',
    'Ext.layout.Fit'
]);


Ext.onReady(function() {

    Ext.viewport.Viewport.setup({
        layout: 'fit'
    });

    Ext.Viewport.add({
        xtype: 'mainview'

    });
});


Ext.define('SpecGenerator.MainView', {
    extend: 'Ext.Container',
    xtype: 'mainview',
    controller: 'main',

    layout: {
        type: 'vbox',
        align: 'stretch'
    },

    items: [{
        xtype: 'panel',
        flex: 1,
        layout: {
            type: 'hbox',
            align: 'stretch'
        },
        defaults: {
            flex: 1
        },
        items: [{
            xtype: 'panel',
            layout: 'fit',
            title: 'input',
            margin: '10 5 10 10',

            items: [{
                xtype: 'textareafield',
                listeners: {
                    change: 'onInputChange'
                }
            }]
        }, {
            xtype: 'panel',
            layout: 'fit',
            title: 'output',
            margin: '10 10 10 5',
            items: [{
                xtype: 'textareafield',
                reference: 'outputField',
                readOnly: true
            }]
        }]
    }, {
        xtype: 'panel',
        reference: 'contentPanel',
        scrollable: true,
        flex: 1,
        margin: 10
    }]
});

Ext.define('SpecGenerator.MainController', {
    extend: 'Ext.app.ViewController',
    alias: 'controller.main',

    ignoredEls: {
        el: 1,
        renderElement: 1,
        innerElement: 1,
        focusEl: 1,
        ariaEl: 1
    },

    onInputChange: function(textArea, value) {
        var cmp = this.renderComponent(value),
            layoutSpec = this.getLayoutSpec(cmp),
            output = '';

        if (layoutSpec) {
            output = 'expect(cmp).toHaveLayout(' + JSON.stringify(layoutSpec, null, 4)
                .replace(/"(\w+)":/g, function (p1, p2) {
                    return p2 + ':';
                })
                .replace(/"/g, "'")
                // .replace(/: \{\n\s+/g, ': { '  )
                // .replace(/\n\s+\}/g, ' }')
                + ');';
        }

        this.lookup('outputField').setValue(output);
    },

    renderComponent: function(code) {
        var contentPanel = this.lookup('contentPanel'),
            fn, cmp, err;

        try {
            fn = new Function(code);
            cmp = fn();
        } catch (e) {
            err = e;
            cmp = Ext.create({
                xtype: 'component',
                html: e
            });
        }

        contentPanel.removeAll(true);

        if (cmp) {
            contentPanel.add(cmp);

            contentPanel.getScrollable().scrollTo(0, 0);
        }

        if (!err) {
            return cmp;
        }
    },

    getLayoutSpec: function(cmp) {
        var me = this,
            references = {},
            ref, spec, name, prop;

        if (cmp) {
            spec = {};

            cmp.referenceList.forEach(function(name) {
                if (!(name in me.ignoredEls)) {
                    ref = cmp[name];
                    spec[name] = me.getElementSpec(ref);
                    references[name] = cmp[name];
                }
            });

            for (name in cmp) {
                if (cmp.hasOwnProperty(name) && !references[name]) {
                    prop = cmp[name];

                    // ignore config backing properties - we're only looking for reference els
                    if (prop && prop.isElement && !(name in me.ignoredEls) && !name.startsWith('_')) {
                        spec[name] = me.getElementSpec(prop);
                    }
                }
            }

            if (cmp instanceof Ext.Container) {
                cmp.getItems().each(function(item, index) {
                    (spec.items || (spec.items = {}))[index] = me.getLayoutSpec(item);
                });
            }
        }

        return spec;

    },

    getElementSpec: function(el) {
        var ct = this.lookup('contentPanel').bodyElement,
            spec = {},
            xywh;

        if (!el.isVisible(true, 1)) {
            // if element is hidden using display there is no point in returning any
            // coordinates or sizes because they will always be 0.
            if (!el.isVisible(true, 2)) {
                spec.dv = false;
            } else {
                spec.d = false;
            }
        } else {
            if (!el.isVisible(true, 2)) {
                spec.v = false;
            }

            spec.xywh = [
                el.getX() - ct.getX(),
                el.getY() - ct.getY(),
                el.getWidth(),
                el.getHeight()
            ].join(' ');
        }

        return spec;
    }

});




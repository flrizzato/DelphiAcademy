describe("Ext.layout.wrapper.BoxDock", function() {
    describe("border management", function() {
        beforeEach(function() {
            this.addMatchers({
                toHaveBorderWidth: function(expected) {
                    var el = this.actual.element || this.actual,
                        error = null,
                        t, r, b, l;

                    if (Ext.isNumber(expected)) {
                        expected += 'px';
                    }

                    if (Ext.isString(expected)) {
                        t = r = b = l = expected;
                    } else if (Ext.isObject(expected)) {
                        t = expected.t || expected.top;
                        r = expected.r || expected.right;
                        b = expected.b || expected.bottom;
                        l = expected.l || expected.left;
                    };

                    expected = {top: t, right: r, bottom: b, left: l};

                    Ext.Object.each(expected, function(key, value) {
                        var style = 'border-' + key + '-width',
                            current = el.getStyle(style);

                        if (current !== value) {
                            error = 'Expected ' + style + ': ' + current + ' to be equal to ' + value;
                            return false;
                        }
                    });

                    this.message = error;

                    return !error;
                }
            });
        });

        Ext.each([true, false], function(enabled) {
            it('should ' + (enabled? '' : 'not') + ' propagate down to all of the nested dock wrappers if ' + enabled, function() {
                var container = Ext.create('Ext.Container', {
                        renderTo: Ext.getBody(),
                        manageBorders: enabled,
                        items: [
                            {docked: 'top'},
                            {docked: 'right'},
                            {docked: 'bottom'},
                            {docked: 'left'},
                            {docked: 'top'},
                            {docked: 'left'}
                        ]
                    }),
                    layout = container.getLayout(),
                    wrapper = layout.getDockWrapper(),
                    i = 0;

                for (; i<6; ++i) {
                    expect(wrapper).toBeDefined();
                    expect(wrapper.getManageBorders()).toBe(enabled);
                    expect(wrapper.getElement().hasCls('x-managed-borders')).toBe(enabled);
                    wrapper = wrapper.getInnerWrapper();
                }

                Ext.destroy(container);
            });
        });

        Ext.each([true, false], function(enabled) {
            it('should ' + (enabled? '' : 'not') + ' remove the inner border of docked items if ' + enabled, function() {
                var areas = ['top', 'right', 'bottom', 'left', 'top', 'left'],
                    container = Ext.create('Ext.Container', {
                        renderTo: Ext.getBody(),
                        manageBorders: enabled,
                        items: areas.map(function(area) {
                            return {
                                docked: area,
                                style: {
                                    border: '2px solid blue'
                                }
                            }
                        })
                    }),
                    expected = {
                        top:    enabled? {t: '2px', r: '2px', b: '0px', l: '2px'} : '2px',
                        right:  enabled? {t: '2px', r: '2px', b: '2px', l: '0px'} : '2px',
                        bottom: enabled? {t: '0px', r: '2px', b: '2px', l: '2px'} : '2px',
                        left:   enabled? {t: '2px', r: '0px', b: '2px', l: '2px'} : '2px'
                    };

                Ext.each(areas, function(area, index) {
                    var item = container.getDockedItems()[index];
                    expect(item).toHaveBorderWidth(expected[area]);
                    expect(item.getDocked()).toBe(area);
                });

                Ext.destroy(container);
            });
        });

        // https://sencha.jira.com/browse/EXT-167
        it('should not remove borders of docked items in nested containers', function() {
            var container = Ext.create('Ext.Container', {
                    renderTo: Ext.getBody(),
                    referenceHolder: true,
                    manageBorders: true,
                    items: [{
                        reference: 'foo',
                        docked: 'left',
                        style: {
                            border: '2px solid blue'
                        }
                    }, {
                        xtype: 'container',
                        items: [{
                            xtype: 'container',
                            reference: 'bar',
                            docked: 'top',
                            style: {
                                border: '2px solid blue'
                            }
                        }]
                    }]
                });

            Ext.Object.each({
                foo: {t: '2px', r: '0px', b: '2px', l: '2px', docked: 'left'},
                bar: {t: '2px', r: '2px', b: '2px', l: '2px', docked: 'top'}
            }, function(key, values) {
                var item = container.lookup(key);
                expect(item.getDocked()).toBe(values.docked);
                expect(item).toHaveBorderWidth(values);
            });

            Ext.destroy(container);
        });
    });
});

topSuite("Ext.Panel", ['Ext.app.ViewModel', 'Ext.Button'], function() {
    var panel,
        items;

    function createPanel (config) {
        if (Ext.isArray(config)) {
            config = {
                items: config
            };
        } else {
            config = Ext.apply({}, config);
        }

        // Render unless floated.
        // Floateds insert themselves into the DOM
        if (!config.floated && !config.hasOwnProperty('renderTo')) {
            config.renderTo = document.body;
        }

        panel = new Ext.Panel(config);

        items = panel.getItems().items;
    }

    afterEach(function () {
        panel = Ext.destroy(panel);
    });

    describe("binding", function() {
        it("should be able to bind items inside the header", function() {
            createPanel({
                viewModel: {
                    data: {
                        foo: 'aTitle'
                    }
                },
                header: {
                    items: {
                        xtype: 'component',
                        bind: '{foo}',
                        itemId: 'foo'
                    }
                }
            });
            panel.getViewModel().notify();
            expect(panel.down('#foo').getHtml()).toBe('aTitle');
        });

        it("should be able to bind the title", function() {
            createPanel({
                viewModel: {
                    data: {
                        foo: 'aTitle'
                    }
                },
                bind: '{foo}'
            });
            panel.getViewModel().notify();
            expect(panel.getHeader().getTitle().getText()).toBe('aTitle');
        });

        it("should inherit a viewmodel correctly when an instance is added", function() {
            var ct = new Ext.Container({
                renderTo: Ext.getBody(),
                viewModel: {
                    data: { theTitle: 'aTitle' }
                }
            });

            createPanel({
                renderTo: null,
                viewModel: {},
                bind: '{theTitle}'
            });

            ct.add(panel);

            var vm = ct.getViewModel();

            expect(panel.getViewModel().getParent()).toBe(vm);
            vm.notify();
            expect(panel.getTitle()).toBe('aTitle');
            ct.destroy();
        });
    });

    describe("configuration", function () {
        describe("title", function () {
            it("should not create a header no title is provided", function () {
                createPanel();
                expect(panel.getHeader()).toBeNull();
            });

            it("should create a header if title is provided", function () {
                createPanel({
                    title: 'Foo'
                });
                expect(panel.getHeader().getTitle().getText()).toBe('Foo');
            });

            it("should not create header if title is provided, but header:false", function () {
                createPanel({
                    title: 'Foo',
                    header: false
                });
                expect(panel.getHeader()).toBeNull();
            });
        });
    });

    describe("header", function() {
        function expectHeaderEl() {
            var el = panel.element.down('.x-panelheader');
            expect(el).not.toBeNull();
        }

        function expectNoHeaderEl() {
            var el = panel.element.down('.x-panelheader');
            expect(el).toBeNull();
        }

        describe("at configuration time", function() {
            it("should not create a header by default", function() {
                createPanel();
                expect(panel.getHeader()).toBeNull();
                expectNoHeaderEl();
            });

            it("should not create a header with header: null", function() {
                createPanel({
                    header: null
                });
                expect(panel.getHeader()).toBeNull();
                expectNoHeaderEl();
            });

            it("should not create a header with header: false", function() {
                createPanel({
                    header: false
                });
                expect(panel.getHeader()).toBeNull();
                expectNoHeaderEl();
            });

            it("should create a header with header: true", function() {
                createPanel({
                    header: true
                });
                expect(panel.getHeader().isXType('panelheader')).toBe(true);
                expectHeaderEl();
            });

            it("should create a header with a config", function() {
                createPanel({
                    header: {
                        title: 'Foo'
                    }
                });
                expect(panel.getHeader().isXType('panelheader')).toBe(true);
                expectHeaderEl();
            });

            it("should create a header with a title config", function() {
                createPanel({
                    title: 'Foo'
                });
                expect(panel.getHeader().isXType('panelheader')).toBe(true);
                expectHeaderEl();
            });
        });

        describe("after configuration", function() {
            describe("no header -> header", function() {
                it("should create a header when passing a string", function() {
                    createPanel();
                    panel.setHeader('Foo');
                    expect(panel.getHeader().isXType('panelheader')).toBe(true);
                    expectHeaderEl();
                });

                it("should create a header when passing a config", function() {
                    createPanel();
                    panel.setHeader({
                        title: 'Foo'
                    });
                    expect(panel.getHeader().isXType('panelheader')).toBe(true);
                    expectHeaderEl();
                });

                it("should create a header when passing true", function() {
                    createPanel();
                    panel.setHeader(true);
                    expect(panel.getHeader().isXType('panelheader')).toBe(true);
                    expectHeaderEl();
                });
            });

            describe("header -> no header", function() {
                it("should destroy the header when passing false", function() {
                    createPanel({
                        title: 'Foo'
                    });
                    var h = panel.getHeader();
                    panel.setHeader(false);
                    expect(h.destroyed).toBe(true);

                    expect(panel.getHeader()).toBeNull();
                    expectNoHeaderEl();
                });

                it("should destroy the header when passing null", function() {
                    createPanel({
                        title: 'Foo'
                    });
                    var h = panel.getHeader();
                    panel.setHeader(null);
                    expect(h.destroyed).toBe(true);

                    expect(panel.getHeader()).toBeNull();
                    expectNoHeaderEl();
                });
            });

            describe("changing header", function() {
                it("should apply extra configs when passing an object", function() {
                    createPanel({
                        title: 'Foo'
                    });
                    panel.setHeader({
                        titleAlign: 'right'
                    });
                    var h = panel.getHeader();
                    expect(h.isXType('panelheader')).toBe(true);
                    expectHeaderEl();
                    expect(h.getTitle().getText()).toBe('Foo');
                    expect(h.getTitleAlign()).toBe('right');
                });

                it("should leave the header in place when passing true", function() {
                    createPanel({
                        title: 'Foo'
                    });
                    panel.setHeader(true);
                    expect(panel.getHeader().isXType('panelheader')).toBe(true);
                    expectHeaderEl();
                });
            });
        });
    });

    describe("methods", function () {
        describe("setTitle", function () {
            it("should update title when a header exists", function () {
                createPanel({
                    title: 'Foo'
                });

                panel.setTitle('Bar');
                expect(panel.getHeader().getTitle().getText()).toBe('Bar');
            });

            it("should not create a header when header:false", function () {
                createPanel({
                    title: 'Foo',
                    header: false
                });

                panel.setTitle('Bar');

                expect(panel.getHeader()).toBeNull();
            });
        });

        describe('showBy', function() {
            var byCmp;

            beforeEach(function() {
                byCmp = new Ext.Button({
                    floated: true,
                    width: 100,
                    text: 'Button'
                });
                byCmp.showAt(0, 10);

                // Use viewport-realative left position so that changing
                // the sandbox width will cause a realignment.
                byCmp.el.setLeft('10%');
            });

            afterEach(function() {
                byCmp = Ext.destroy(byCmp);
                top.Test.SandBox.getIframe().style.width = '';
            });

            it('should show at the correct position, and realign on viewport resize', function() {
                createPanel({
                    title: 'The title',
                    html: 'The content',
                    floated: true
                });
                panel.showBy(byCmp, 'tl-bl');
                expect(panel.el.getX()).toBe(byCmp.el.getX());
                expect(panel.el.getY()).toBe(byCmp.el.getY() + byCmp.el.getHeight());

                top.Test.SandBox.getIframe().style.width = '900px';

                // Wait for async resize event to fire.
                waits(100);

                // The viewport resize listener should kick in and maintain
                // the alignment.
                runs(function() {
                    expect(panel.el.getX()).toBe(byCmp.el.getX());
                    expect(panel.el.getY()).toBe(byCmp.el.getY() + byCmp.el.getHeight());
                });
            });

            it('should be able to show a floated menu after hiding it', function() {
                createPanel({
                    title: 'The title',
                    html: 'The content',

                    // Prevent createPanel from autoRendering so that we excercise
                    // the "become floating on alignTo" capability.
                    renderTo: null
                });

                panel.showBy(byCmp);
                panel.hide();
                panel.showBy(byCmp);

                expect(panel.getFloated()).toBe(true);
                expect(panel.isVisible()).toBe(true);
            });

            // https://sencha.jira.com/browse/EXTJS-25755
            it('should be able to show a floated menu at the correct place if its align target has changed position', function() {
                var animateEndSpy = jasmine.createSpy();

                createPanel({
                    title: 'The title',
                    html: 'The content',
                    floated: true
                });

                panel.showBy(byCmp, 'tl-bl');
                expect(panel.el.getX()).toBe(byCmp.el.getX());
                expect(panel.el.getY()).toBe(byCmp.el.getY() + byCmp.el.getHeight());
                panel.hide();

                byCmp.setXY(100, 100);
                panel.setShowAnimation({
                    listeners: {
                        animationend: function() {
                            // Should show up in the correct place having been shown and pre-aligned
                            // in preprocessShow, and animated directly into the correct position.
                            expect(panel.el.getX()).toBe(byCmp.el.getX() - panel.el.getWidth());
                            expect(panel.el.getY()).toBe(byCmp.el.getY() - panel.el.getHeight());
                            animateEndSpy();
                        }
                    }
                });
                panel.showBy(byCmp, 'br-tl');

                waitsForSpy(animateEndSpy);
            });

            it('should use a default showBy alignment of t-b', function() {
                createPanel({
                    title: 'The title',
                    html: 'The content',
                    floated: true
                });
                panel.showBy(byCmp);
                expect(panel.el.getX()).toBe(byCmp.el.getAnchorXY('b')[0] - panel.el.getWidth() / 2);
                expect(panel.el.getY()).toBe(byCmp.el.getRegion().bottom);
            });
        });
    });

    describe("bodyStyle", function() {
        it("should initialize style using a string", function () {
            createPanel({
                bodyStyle: 'position: absolute; cursor: pointer'
            });

            expect(panel.bodyElement.getStyle('position')).toBe('absolute');
            expect(panel.bodyElement.getStyle('cursor')).toBe('pointer');
        });

        it("should initialize bodyStyle using an object", function () {
            createPanel({
                bodyStyle: {
                    position: 'absolute',
                    cursor: 'pointer'
                }
            });

            expect(panel.bodyElement.getStyle('position')).toBe('absolute');
            expect(panel.bodyElement.getStyle('cursor')).toBe('pointer');
        });

        it("should set bodyStyle using a string", function () {
            createPanel();

            panel.setBodyStyle('position: absolute; cursor: pointer');

            expect(panel.bodyElement.getStyle('position')).toBe('absolute');
            expect(panel.bodyElement.getStyle('cursor')).toBe('pointer');
        });

        it("should set bodyStyle using an object", function () {
            createPanel();

            panel.setBodyStyle({
                position: 'absolute',
                cursor: 'pointer'
            });

            expect(panel.bodyElement.getStyle('position')).toBe('absolute');
            expect(panel.bodyElement.getStyle('cursor')).toBe('pointer');
        });

        it("should throw an error when getStyle is called", function () {
            createPanel();

            expect(function () {
                panel.getBodyStyle();
            }).toThrow("'bodyStyle' is a write-only config.  To query element styles use the Ext.dom.Element API.");
        });
    });

    describe("headerPosition", function() {
        var size = 400,
            measured;

        beforeEach(function() {
            if (!measured) {
                makeHeaderPanel('top');
                measured = panel.getHeader().element.getHeight();
                panel = Ext.destroy(panel);
            }
        });

        function makeHeaderPanel(pos) {
            createPanel({
                width: size,
                height: size,
                title: 'Panel Title',
                headerPosition: pos || 'top'
            });
        }

        function getBox() {
            return panel.getHeader().element.getRegion();
        }

        function expectBox(t, r, b, l) {
            var box = getBox();
            expect(box.top).toBe(t);
            expect(box.right).toBe(r);
            expect(box.bottom).toBe(b);
            expect(box.left).toBe(l);
        }

        function expectTop() {
            expectBox(0, size, measured, 0);
        }

        function expectRight() {
            expectBox(0, size, size, size - measured);
        }

        function expectBottom() {
            expectBox(size - measured, size, size, 0);
        }

        function expectLeft() {
            expectBox(0, measured, size, 0);
        }

        describe("initial configuration", function() {
            describe("top", function() {
                it("should position correctly", function() {
                    makeHeaderPanel('top');
                    expectTop();
                });
            });

            describe("right", function() {
                it("should position correctly", function() {
                    makeHeaderPanel('right');
                    expectRight();
                });
            });

            describe("bottom", function() {
                it("should position correctly", function() {
                    makeHeaderPanel('bottom');
                    expectBottom();
                });
            });

            describe("left", function() {
                it("should position correctly", function() {
                    makeHeaderPanel('left');
                    expectLeft();
                });
            });
        });

        describe("dynamic", function() {
            describe("from top", function() {
                beforeEach(function() {
                    makeHeaderPanel('top');
                });

                it("should move to the right", function() {
                    panel.setHeaderPosition('right');
                    expectRight();
                });

                it("should move to the bottom", function() {
                    panel.setHeaderPosition('bottom');
                    expectBottom();
                });

                it("should move to the left", function() {
                    panel.setHeaderPosition('left');
                    expectLeft();
                });
            });

            describe("from right", function() {
                beforeEach(function() {
                    makeHeaderPanel('right');
                });

                it("should move to the top", function() {
                    panel.setHeaderPosition('top');
                    expectTop();
                });

                it("should move to the bottom", function() {
                    panel.setHeaderPosition('bottom');
                    expectBottom();
                });

                it("should move to the left", function() {
                    panel.setHeaderPosition('left');
                    expectLeft();
                });
            });

            describe("from bottom", function() {
                beforeEach(function() {
                    makeHeaderPanel('bottom');
                });

                it("should move to the top", function() {
                    panel.setHeaderPosition('top');
                    expectTop();
                });

                it("should move to the right", function() {
                    panel.setHeaderPosition('right');
                    expectRight();
                });

                it("should move to the left", function() {
                    panel.setHeaderPosition('left');
                    expectLeft();
                });
            });

            describe("from left", function() {
                beforeEach(function() {
                    makeHeaderPanel('left');
                });

                it("should move to the top", function() {
                    panel.setHeaderPosition('top');
                    expectTop();
                });

                it("should move to the right", function() {
                    panel.setHeaderPosition('right');
                    expectRight();
                });

                it("should move to the bottom", function() {
                    panel.setHeaderPosition('bottom');
                    expectBottom();
                });
            });
        });
    });

    describe("ui", function() {
        var header, title;
        beforeEach(function() {
            createPanel({
                html: 'Panel with ui',
                ui: 'foobar',
                flex: 1,
                header: {
                    items: [{
                        xtype: 'button',
                        text: 'Foo'
                    }, {
                        xtype: 'button',
                        text: 'Bar'
                    }]
                }
            });

            header = panel.getHeader();
        });

        afterEach(function() {
            Ext.destroy(panel);
            header = null;
            title = null;
        });

        it("should pass the ui to the header when creating it later as an object", function() {
            panel.setTitle({
                text: 'Foo'
            });
            title =  header.getTitle();

            expect(title.getText()).toBe('Foo');
            expect(title.getUi()).toBe('foobar');
        });

        it("should pass the ui to the header when creating it later as a string", function() {
            panel.setTitle('Foo');
            title =  header.getTitle();

            expect(title.getText()).toBe('Foo');
            expect(title.getUi()).toBe('foobar');
        });
    });

    describe('convenient docked configs', function () {
        function createDockTest (property, docked, options) {
            options = options || {};

            describe(property, function () {
                it('should add docked toolbar when specified as an array', function() {
                    var config = {
                        referenceHolder: true,
                        viewModel: {
                            data: {
                                btnText: 'Bar'
                            }
                        }
                    };

                    config[property] = [{
                        itemId: 'compB',
                        reference: 'b',
                        bind: '{btnText}'
                    }];

                    createPanel(config);
                    panel.getViewModel().notify();

                    expect(panel.child('toolbar').getDocked()).toBe(docked);

                    if (options.ui) {
                        expect(panel.child('toolbar').getUi()).toBe(options.ui);
                    }

                    expect(panel.lookup('b')).toBe(panel.down('#compB'));
                    expect(panel.lookup('b').getText()).toBe('Bar');
                });

                it('should add docked toolbar when specified as a config', function() {
                    var config = {
                        referenceHolder: true,
                        viewModel: {
                            data: {
                                btnText: 'Bar'
                            }
                        }
                    };

                    config[property] = {
                        xtype: 'toolbar',
                        itemId: 'compA',
                        reference: 'a',
                        items: [{
                            itemId: 'compB',
                            reference: 'b',
                            bind: '{btnText}'
                        }]
                    };

                    createPanel(config);

                    panel.getViewModel().notify();

                    var tb = panel.lookup('a');
                    var item = panel.lookup('b');
                    var ca = panel.down('#compA');

                    expect(tb).toBe(ca);
                    expect(tb.isXType('toolbar')).toBe(true);
                    expect(tb.getDocked()).toBe(docked);

                    if (options.ui) {
                        expect(panel.child('toolbar').getUi()).toBe(options.ui);
                    }

                    expect(item).toBe(panel.down('#compB'));
                    expect(item.getText()).toBe('Bar');
                });

                it("should not create a toolbar if the config is set to null", function() {
                    var config = {};
                    config[property] = null;

                    createPanel(config);
                    expect(panel.down('toolbar')).toBeNull();
                });

                describe("dynamic", function() {
                    var setter = Ext.Config.get(property).names.set;
                    describe("setting a value", function() {
                        it("should be able to set an array", function() {
                            createPanel();
                            panel[setter]([{
                                id: 'a',
                                text: 'Foo'
                            }]);

                            var tb = panel.down('toolbar');
                            expect(tb).not.toBeNull();
                            expect(tb.isAncestor(Ext.getCmp('a'))).toBe(true);
                        });

                        it("should be able to set an object", function() {
                            createPanel();
                            panel[setter]({
                                itemId: 'tb',
                                items: [{
                                    id: 'a',
                                    text: 'Foo'
                                }]
                            });

                            var tb = panel.down('toolbar');
                            expect(tb).not.toBeNull();
                            expect(panel.down('#tb')).toBe(tb);
                            expect(tb.isAncestor(Ext.getCmp('a'))).toBe(true);
                        });
                    });

                    describe("clearing a value", function() {
                        it("should be able to clear the value", function() {
                            var config = {},
                                tb;

                            config[property] = [{
                                text: 'Foo'
                            }];
                            createPanel(config);
                            tb = panel.down('toolbar');
                            panel[setter](null);
                            expect(tb.destroyed).toBe(true);
                            expect(panel.down('toolbar')).toBeNull();

                        });
                    });

                    describe("changing a value", function() {
                        it("should be able to set an array", function() {
                            var config = {},
                                old, tb;

                            config[property] = [{
                                text: 'OldButton'
                            }];

                            createPanel(config);
                            old = panel.down('toolbar');
                            panel[setter]([{
                                id: 'a',
                                text: 'Foo'
                            }]);

                            tb = panel.down('toolbar');
                            expect(old.destroyed).toBe(true);
                            expect(tb).not.toBeNull();
                            expect(tb).not.toBe(old);
                            expect(tb.isAncestor(Ext.getCmp('a'))).toBe(true);
                        });

                        it("should be able to set an object", function() {
                            var config = {},
                                old, tb;

                            config[property] = [{
                                text: 'OldButton'
                            }];

                            createPanel(config);
                            old = panel.down('toolbar');
                            panel[setter]({
                                itemId: 'tb',
                                items: [{
                                    id: 'a',
                                    text: 'Foo'
                                }]
                            });

                            tb = panel.down('toolbar');
                            expect(tb).not.toBeNull();
                            expect(panel.down('#tb')).toBe(tb);
                            expect(tb).not.toBe(old);
                            expect(tb.isAncestor(Ext.getCmp('a'))).toBe(true);
                        });
                    });
                });

                if (options.buttonAlign) {
                    it('should use buttonAlign config with an array', function() {
                        var config = {
                            referenceHolder: true,
                            buttonAlign: 'left'
                        };

                        config[property] = [{
                            text: 'Foo',
                            itemId: 'compB',
                            reference: 'b'
                        }];

                        createPanel(config);

                        expect(panel.child('toolbar').getDocked()).toBe('bottom');
                        expect(panel.child('toolbar').getLayout().getPack()).toBe('start');

                        if (options.ui) {
                            expect(panel.child('toolbar').getUi()).toBe(options.ui);
                        }

                        expect(panel.lookup('b')).toBe(panel.down('#compB'));
                    });

                    it('should use buttonAlign config with a config', function() {
                        var config = {
                            referenceHolder: true,
                            buttonAlign: 'center'
                        };

                        config[property] = {
                            xtype: 'toolbar',
                            itemId: 'compA',
                            reference: 'a>',
                            items: [{
                                text: 'Foo',
                                itemId: 'compB',
                                reference: 'b'
                            }]
                        };

                        createPanel(config);

                        expect(panel.lookup('a').getDocked()).toBe('bottom');
                        expect(panel.lookup('a').getLayout().getPack()).toBe('center');

                        if (options.ui) {
                            expect(panel.child('toolbar').getUi()).toBe(options.ui);
                        }

                        expect(panel.lookup('a.b')).toBe(panel.down('#compB'));
                    });
                }
            });
        }

        createDockTest('bbar',    'bottom');
        createDockTest('buttons', 'bottom', { buttonAlign: true, ui: 'footer' });
        createDockTest('lbar',    'left');
        createDockTest('rbar',    'right');
        createDockTest('tbar',    'top');
    });
});

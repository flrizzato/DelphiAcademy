topSuite("Ext.Dialog",
    ['Ext.app.ViewModel', 'Ext.layout.Form', 'Ext.Button', 'Ext.field.Text',
     'Ext.plugin.TabGuard', 'Ext.Dialog'],
function() {
    var dialog;

    function createDialog (config) {
        if (Ext.isArray(config)) {
            config = {
                items: config
            };
        } else {
            config = Ext.apply({}, config);
        }

        dialog = new Ext.Dialog(config);
    }

    function animates (comp) {
        return new Ext.Promise(function (resolve) {
            var anim = comp.activeAnimation;

            if (anim) {
                anim.on({
                    animationend: function () {
                        resolve();
                    }
                });
            }
            else {
                resolve();
            }
        });
    }

    afterEach(function() {
        dialog = Ext.destroy(dialog);
    });

    describe("construction", function() {
        it("should display if displayed: true", function() {
            createDialog({
                displayed: true
            });

            expect(dialog.isHidden()).toBe(false);
        });

        it("should display if hidden: false", function() {
            createDialog({
                hidden: false
            });

            expect(dialog.isHidden()).toBe(false);
        });

        it("should not fire hiddenchange if displayed: true", function() {
            var flag = false;

            createDialog({
                displayed: true,
                listeners: {
                    hiddenchange: function() {
                        flag = true;
                    }
                }
            });

            expect(dialog.isHidden()).toBe(false);
            expect(flag).toBe(false);
        });

        it("should be present in the DOM and positioned correctly", function() {
            var xy, size, x, y;

            createDialog({
                title: 'Dialog',
                html: 'I am not hidden',
                hidden: false
            });

            expect(!!dialog.isCentered()).toBe(true);
            xy = dialog.element.getXY();
            size = dialog.getSize();
            x = Math.round((Ext.getViewportWidth() - size.width)/2);
            y = Math.round((Ext.getViewportHeight() - size.height)/2);
            expect(xy[0]).toBeWithin(1, x);
            expect(xy[1]).toBeWithin(1, y);
            expect(dialog.getFloatWrap().dom.offsetParent.id).toBe('ext-global-floatWrap');
        });
    });

    describe('maximizable:true', function () {
        beforeEach(function () {
            createDialog({
                title: 'Test',
                width: 300,
                height: 200,
                maximizable: true
            });
        });

        it('should have a maximize tool', function () {
            var tool = dialog.down('tool[type=maximize]');

            expect(!tool).toBe(false);
        });

        it('should have proper tools when maximized', function () {
            dialog.maximize(/*animation=*/null);

            var tool = dialog.down('tool[type=restore]');
            expect(!tool).toBe(false);

            tool = dialog.down('tool[type=maximize]');
            expect(tool.getHidden()).toBe(true);
        });

        it('should have proper tools when restored', function () {
            dialog.maximize(/*animation=*/null);
            dialog.restore(/*animation=*/null);

            var tool = dialog.down('tool[type=restore]');
            expect(!tool).toBe(true);

            tool = dialog.down('tool[type=maximize]');
            expect(tool.getHidden()).toBe(false);
        });

        it('should provide promises with maximize and restore', function (done) {
            dialog.setXY(100, 100);
            dialog.show(null);

            dialog.maximize().then(function () {
                expect(dialog.hasCls('x-maximized')).toBe(true);

                dialog.restore().then(function () {
                    expect(dialog.hasCls('x-maximized')).toBe(false);

                    done();
                });
            });
        });

        // https://sencha.jira.com/browse/EXTJS-24989
        // Hide animation never finished because the translateX/translateY animations
        // were competing and losing against the "!important" maximized styles
        it('should be able to animate hide from maximized', function() {
            dialog.show(false);
            dialog.maximize(false);

            dialog.hide();

            // Hidden is only flipped when the hide animation finishes
            waitsFor(function() {
                return dialog.getHidden();
            });
        });
    });

    describe('maximizable:false', function () {
        beforeEach(function () {
            createDialog({
            });
        });

        it('should have no tools', function () {
            var tool = dialog.down('tool[type=maximize]');

            expect(!tool).toBe(true);

            tool = dialog.down('tool[type=restore]');
            expect(!tool).toBe(true);
        });

        it('should have no tools when maximized', function () {
            dialog.maximize(/*animation=*/null);

            var tool = dialog.down('tool[type=restore]');
            expect(!tool).toBe(true);

            tool = dialog.down('tool[type=maximize]');
            expect(!tool).toBe(true);
        });

        it('should still have no tools when restored', function () {
            dialog.restore(/*animation=*/null);

            var tool = dialog.down('tool[type=restore]');
            expect(!tool).toBe(true);

            tool = dialog.down('tool[type=maximize]');
            expect(!tool).toBe(true);
        });

        it('should provide promises with maximize and restore', function (done) {
            dialog.setXY(100, 100);
            dialog.show(null);

            dialog.maximize().then(function () {
                expect(dialog.hasCls('x-maximized')).toBe(true);

                dialog.restore().then(function () {
                    expect(dialog.hasCls('x-maximized')).toBe(false);

                    done();
                });
            });
        });
    });

    describe('maximizable:false maximized:true', function () {
        beforeEach(function () {
            createDialog({
                title: 'Test',
                width: 300,
                height: 200,
                x: 100,
                y: 100,
                centered: false,
                maximized: true
            });
        });

        it('should have no tools', function () {
            var tool = dialog.down('tool[type=maximize]');

            expect(!tool).toBe(true);

            tool = dialog.down('tool[type=restore]');
            expect(!tool).toBe(true);
        });

        it('should still have no tools when restored', function () {
            dialog.restore(/*animation=*/null);

            var tool = dialog.down('tool[type=restore]');
            expect(!tool).toBe(true);

            tool = dialog.down('tool[type=maximize]');
            expect(!tool).toBe(true);
        });

        it('should provide promises with maximize and restore', function (done) {
            dialog.show(null);

            dialog.restore().then(function (restored) {
                expect(restored).toBe(true);
                expect(dialog.hasCls('x-maximized')).toBe(false);

                dialog.maximize().then(function (maximized) {
                    expect(maximized).toBe(true);
                    expect(dialog.hasCls('x-maximized')).toBe(true);

                    done();
                });
            });
        });
    });

    describe('header:false', function () {
        describe('maximizable:true', function () {
            beforeEach(function () {
                createDialog({
                    header: false,
                    maximizable: true
                });
            });

            it('should have a maximize tool', function () {
                var tool = dialog.down('tool[type=maximize]');

                expect(!tool).toBe(true);
            });

            it('should have no tools when maximized', function () {
                dialog.maximize(/*animation=*/null);

                var tool = dialog.down('tool[type=restore]');
                expect(!tool).toBe(true);

                tool = dialog.down('tool[type=maximize]');
                expect(!tool).toBe(true);
            });

            it('should still have tools when restored', function () {
                dialog.maximize(/*animation=*/null);
                dialog.restore(/*animation=*/null);

                var tool = dialog.down('tool[type=restore]');
                expect(!tool).toBe(true);

                tool = dialog.down('tool[type=maximize]');
                expect(!tool).toBe(true);
            });
        });

        describe('maximizable:false', function () {
            beforeEach(function () {
                createDialog({
                    header: false
                });
            });

            it('should have no tools', function () {
                var tool = dialog.down('tool[type=maximize]');

                expect(!tool).toBe(true);

                tool = dialog.down('tool[type=restore]');
                expect(!tool).toBe(true);
            });

            it('should have no tools when maximized', function () {
                dialog.maximize(/*animation=*/null);

                var tool = dialog.down('tool[type=restore]');
                expect(!tool).toBe(true);

                tool = dialog.down('tool[type=maximize]');
                expect(!tool).toBe(true);
            });

            it('should still have no tools when restored', function () {
                dialog.restore(/*animation=*/null);

                var tool = dialog.down('tool[type=restore]');
                expect(!tool).toBe(true);

                tool = dialog.down('tool[type=maximize]');
                expect(!tool).toBe(true);
            });
        });

        describe('maximizable:false maximized:true', function () {
            beforeEach(function () {
                createDialog({
                    header: false,
                    maximized: true
                });
            });

            it('should have no tools', function () {
                var tool = dialog.down('tool[type=maximize]');

                expect(!tool).toBe(true);

                tool = dialog.down('tool[type=restore]');
                expect(!tool).toBe(true);
            });

            it('should still have no tools when restored', function () {
                dialog.restore(/*animation=*/null);

                var tool = dialog.down('tool[type=restore]');
                expect(!tool).toBe(true);

                tool = dialog.down('tool[type=maximize]');
                expect(!tool).toBe(true);
            });
        });
    }); // header:false

    describe('events', function () {
        var events, returnValue;

        function createHandler (ev) {
            return function () {
                events[ev].push(Ext.Array.slice(arguments));
                return returnValue && returnValue[ev];
            };
        }

        beforeEach(function () {
            createDialog({
                title: 'Test',
                x: 100,
                y: 100,
                centered: false,
                width: 300,
                height: 200,
                maximizable: true,
                listeners: {
                    beforemaximize: createHandler('beforemaximize'),
                    beforerestore: createHandler('beforerestore'),
                    maximize: createHandler('maximize'),
                    restore: createHandler('restore')
                }
            });

            events = {
                beforemaximize: [],
                beforerestore: [],
                maximize: [],
                restore: []
            };

            returnValue = null;
        });

        it('should fire events for maximize and restore', function (done) {
            dialog.show();

            animates(dialog).then(function () {
                dialog.maximize().then(function (result) {
                    expect(result).toBe(true);
                    expect(dialog.getMaximized()).toBe(true);

                    expect(events.beforemaximize.length).toBe(1);
                    expect(events.beforerestore.length).toBe(0);
                    expect(events.maximize.length).toBe(1);
                    expect(events.restore.length).toBe(0);

                    expect(events.beforemaximize[0][0]).toBe(dialog);
                    expect(events.maximize[0][0]).toBe(dialog);


                    dialog.restore().then(function (result) {
                        expect(result).toBe(true);
                        expect(dialog.getMaximized()).toBe(false);

                        expect(events.beforemaximize.length).toBe(1);
                        expect(events.beforerestore.length).toBe(1);
                        expect(events.maximize.length).toBe(1);
                        expect(events.restore.length).toBe(1);

                        expect(events.beforemaximize[0][0]).toBe(dialog);
                        expect(events.beforerestore[0][0]).toBe(dialog);
                        expect(events.maximize[0][0]).toBe(dialog);
                        expect(events.restore[0][0]).toBe(dialog);

                        done();
                    });
                });
            });
        });

        it('should abort maximize if beforemaximize returns false', function (done) {
            dialog.show();

            animates(dialog).then(function () {
                returnValue = {
                    beforemaximize: false
                };

                dialog.maximize().then(function (result) {
                    expect(result).toBe(false);
                    expect(dialog.getMaximized()).toBeFalsy();

                    expect(events.beforemaximize.length).toBe(1);
                    expect(events.beforerestore.length).toBe(0);
                    expect(events.maximize.length).toBe(0);
                    expect(events.restore.length).toBe(0);

                    expect(events.beforemaximize[0][0]).toBe(dialog);
                    done();
                });
            });
        });

        it('should abort restore if beforerestore returns false', function (done) {
            dialog.show();

            animates(dialog).then(function () {
                dialog.maximize(null);

                expect(events.beforemaximize.length).toBe(1);
                expect(events.beforerestore.length).toBe(0);
                expect(events.maximize.length).toBe(1);
                expect(events.restore.length).toBe(0);

                expect(events.beforemaximize[0][0]).toBe(dialog);
                expect(events.maximize[0][0]).toBe(dialog);

                returnValue = {
                    beforerestore: false
                };

                dialog.restore().then(function (result) {
                    expect(result).toBe(false);
                    expect(dialog.getMaximized()).toBe(true);

                    expect(events.beforemaximize.length).toBe(1);
                    expect(events.beforerestore.length).toBe(1);
                    expect(events.maximize.length).toBe(1);
                    expect(events.restore.length).toBe(0);

                    expect(events.beforemaximize[0][0]).toBe(dialog);
                    expect(events.beforerestore[0][0]).toBe(dialog);
                    expect(events.maximize[0][0]).toBe(dialog);

                    done();
                });
            });
        });
    });
    
    describe("tab guards", function() {
        var before, after;
        
        afterEach(function() {
            Ext.destroy(before, after);
            before = after = null;
        });
        
        function makeDialog(config, noWaitForShow) {
            config = Ext.apply({
                tabGuard: true,
                centered: false,
                x: 10,
                y: 40,
                hideAnimation: null,
                showAnimation: null
            }, config);
            
            createDialog(config);
            
            if (!noWaitForShow) {
                var showSpy = jasmine.createSpy('dialog show');
                
                dialog.on('show', showSpy);
                dialog.show();
                
                waitForSpy(showSpy);
            }
            
            return dialog;
        }
        
        function makeButton(config, noWaitForShow) {
            var button;
            
            config = Ext.apply({
                floated: true,
                x: 10,
                y: 0
            }, config);
            
            button = new Ext.Button(config);
            
            if (!noWaitForShow) {
                var showSpy = jasmine.createSpy(
                    (config.text || config.id || config.itemId || 'button') + ' show'
                );
                
                button.on('show', showSpy);
                button.show();
                
                waitForSpy(showSpy);
            }
            
            return button;
        }
        
        describe("initTabGuards", function() {
            function expectTabbables(numberOfEls) {
                var tabbables = dialog.el.findTabbableElements({
                    skipSelf: true
                });
                
                expect(tabbables.length).toBe(numberOfEls);
            }
            
            describe("initially empty dialog", function() {
                beforeEach(function() {
                    makeDialog({
                        title: 'frobbe',
                        closable: false
                    });
                });
                
                it("should not set up tab guards", function() {
                    expectTabbables(0);
                });
                
                it("should add tab guards when tool is added", function() {
                    dialog.addTool({ type: 'pin' });
                    
                    // 2 dialog guards + 1 tabbable tool
                    expectTabbables(3);
                });
                
                it("should add tab guards when an item is docked", function() {
                    dialog.add({
                        xtype: 'button',
                        text: 'foo',
                        docked: 'bottom'
                    });
                    
                    expectTabbables(3);
                });
                
                it("should add tab guards when a child component is added", function() {
                    dialog.add({
                        xtype: 'textfield',
                        fieldLabel: 'Throbbe'
                    });
                    
                    expectTabbables(3);
                });
            });
            
            describe("dialog becoming empty", function() {
                describe("removing items", function() {
                    it("should disarm tab guards when last item is removed", function() {
                        makeDialog({
                            title: 'guzzard',
                            closable: false,
                            items: [{
                                xtype: 'button',
                                text: 'frobbe'
                            }]
                        });
                        
                        runs(function() {
                            var btn = dialog.down('button');
                            
                            dialog.remove(btn, true);
                            
                            expectTabbables(0);
                        });
                    });
                    
                    it("should disarm tab guards when last docked item is removed", function() {
                        makeDialog({
                            title: 'blerg',
                            closable: false,
                            items: [{
                                xtype: 'button',
                                text: 'sploosh!',
                                docked: 'bottom'
                            }]
                        });
                        
                        runs(function() {
                            var btn = dialog.down('button');
                            
                            dialog.remove(btn, true);
                            
                            expectTabbables(0);
                        });
                    });
                });
            });
        });
        
        describe("ARIA attributes", function() {
            function makeAttrSuite(position, active) {
                describe(position + " guard", function() {
                    var guard;
                    
                    beforeEach(function() {
                        guard = position === 'top' ? dialog.tabGuardBeforeEl : dialog.tabGuardAfterEl;
                    });
                
                    it("should " + (active ? "" : "not ") + "have tabindex", function() {
                        expect(guard.isTabbable()).toBe(!!active);
                    });
            
                    it("should have aria-hidden", function() {
                        expect(guard).toHaveAttr('aria-hidden', 'true');
                    });
                    
                    // It is important that tab guards are not published
                    // to Assistive Technologies as announceable entities,
                    // hence the tests.
                    it("should have no title", function() {
                        expect(guard).not.toHaveAttr('title');
                    });
                    
                    it("should not have aria-label", function() {
                        expect(guard).not.toHaveAttr('aria-label');
                    });
                    
                    it("should not have aria-labelledby", function() {
                        expect(guard).not.toHaveAttr('aria-labelledby');
                    });
                    
                    it("should have no aria-describedby", function() {
                        expect(guard).not.toHaveAttr('aria-describedby');
                    });
                });
            }
            
            describe("with no tabbable elements", function() {
                beforeEach(function() {
                    makeDialog({
                        collapsible: false,
                        closable: false
                    });
                });
                
                makeAttrSuite('top');
                makeAttrSuite('bottom');
            });
            
            describe("with tabbable elements", function() {
                beforeEach(function() {
                    makeDialog({
                        collapsible: true,
                        closable: true
                    });
                });
                
                makeAttrSuite('top', true);
                makeAttrSuite('bottom', true);
            });
        });
        
        // We repeat almost the same set of tests for both modal
        // and non-modal dialogs under the assumption that things
        // may not go according to plan and focus can somehow
        // get under the skin of the modal mask. This case,
        // however unlikely, should also be handled gracefully.
        function makeTabSuite(modal) {
            var pressTab = jasmine.pressTabKey,
                expectFocused = jasmine.expectFocused,
                tool, fooField, barField, okBtn, cancelBtn;
            
            describe("tabbing with focusables inside, modal: " + modal, function() {
                beforeEach(function() {
                    before = makeButton({
                        id: 'beforeButton',
                        text: 'before'
                    });
                    
                    makeDialog({
                        title: 'foo',
                        
                        modal: modal,
                        
                        // This will add tools and make the header a toolbar;
                        // we need this to test that the top tab guard is indeed
                        // at the top of the tab order above the dialog header.
                        minimizable: true,
                        maximizable: true,
                        
                        layout: 'form',
                        
                        items: [{
                            xtype: 'textfield',
                            name: 'foo',
                            fieldLabel: 'foo'
                        }, {
                            xtype: 'textfield',
                            name: 'bar',
                            fieldLabel: 'bar'
                        }],
                        
                        // Buttons toolbar is there to test that bottom tab guard
                        // is below it in the tab order.
                        buttons: [{
                            text: 'OK'
                        }, {
                            text: 'Cancel'
                        }]
                    });
                    
                    tool = dialog.down('tool');
                    fooField = dialog.down('textfield[name=foo]');
                    barField = dialog.down('textfield[name=bar]');
                    okBtn = dialog.down('button[text=OK]');
                    cancelBtn = dialog.down('button[text=Cancel]');
                    
                    after = makeButton({
                        id: 'afterButton',
                        text: 'after',
                        y: 300
                    });
                });
                
                describe("from outside the dialog", function() {
                    it("should tab from before button to the first tool", function() {
                        pressTab(before, true);

                        runs(function() {
                            expectFocused(tool);
                        });
                    });
                    
                    it("should shift-tab from after button to the Cancel button", function() {
                        pressTab(after, false);

                        runs(function() {
                            expectFocused(cancelBtn);
                        });
                    });
                });
                
                TODO('Modern dialogs are not yet focusable by default').
                describe("from dialog", function() {
                    it("should tab to the first tool", function() {
                        pressTab(dialog, true);

                        runs(function() {
                            expectFocused(tool);
                        });
                    });
                });
                
                describe("within dialog", function() {
                    describe("forward", function() {
                        it("should tab from first tool to the foo field", function() {
                            pressTab(tool, true);

                            runs(function() {
                                expectFocused(fooField);
                            });
                        });
                        
                        it("should tab from foo field to bar field", function() {
                            pressTab(fooField, true);

                            runs(function() {
                                expectFocused(barField);
                            });
                        });
                        
                        it("should tab from bar field to OK button", function() {
                            pressTab(barField, true);

                            runs(function() {
                                expectFocused(okBtn);
                            });
                        });
                        
                        it("should tab from OK button to Cancel button", function() {
                            pressTab(okBtn, true);

                            runs(function() {
                                expectFocused(cancelBtn);
                            });
                        });
                        
                        it("should tab from Cancel button back to the first tool", function() {
                            pressTab(cancelBtn, true);

                            runs(function() {
                                expectFocused(tool);
                            });
                        });
                    });
                    
                    describe("backward", function() {
                        it("should shift-tab from Cancel button to OK button", function() {
                            pressTab(cancelBtn, false);

                            runs(function() {
                                expectFocused(okBtn);
                            });
                        });
                        
                        it("should shift-tab from Ok button to bar field", function() {
                            pressTab(okBtn, false);

                            runs(function() {
                                expectFocused(barField);
                            });
                        });
                        
                        it("should shift-tab from bar field to foo field", function() {
                            pressTab(barField, false);

                            runs(function() {
                                expectFocused(fooField);
                            });
                        });
                        
                        it("should shift-tab from foo field to the first tool", function() {
                            pressTab(fooField, false);

                            runs(function() {
                                expectFocused(tool);
                            });
                        });
                        
                        it("should shift-tab from the first tool back to Cancel button", function() {
                            pressTab(tool, false);

                            runs(function() {
                                expectFocused(cancelBtn);
                            });
                        });
                    });
                });
            });
            
            // Modal dialog will mask all elements below its own el, so tabbing
            // to and fro does not make any sense
            if (!modal) {
                describe("tabbing with no focusables", function() {
                    beforeEach(function() {
                        before = makeButton({
                            id: 'beforeButton',
                            text: 'before'
                        });
                        
                        // This dialog should have no tools at all
                        makeDialog({
                            title: 'bar',
                            
                            modal: modal,
                            closable: false,
                            draggable: false
                        });
                        
                        after = makeButton({
                            id: 'afterButton',
                            text: 'after',
                            y: 300
                        });
                    });
                    
                    describe("from outside the dialog", function() {
                        it("should tab from before button to the after button", function() {
                            pressTab(before, true);
                            
                            expectFocused(after);
                        });
                        
                        it("should shift-tab from after button to the before button", function() {
                            pressTab(after, false);
                            
                            expectFocused(before);
                        });
                    });
                });
            }
        }

        makeTabSuite(false);
        makeTabSuite(true);
    });

    describe('centered after an animated show', function() {
        var showSpy = jasmine.createSpy();

        beforeEach(function () {
            createDialog({
                title: 'Test',
                width: 300,
                height: 200,
                listeners: {
                    show: showSpy
                }
            });
        });

        it('should be centered after an animated show', function() {
            dialog.show();

            waitsForSpy(showSpy);

            runs(function() {
                var position = [dialog.getX(), dialog.getY()];

                // This operation must not move the dialog. It must have shows in
                // exactly the correct position.
                dialog.center();

                expect([dialog.getX(), dialog.getY()]).toEqual(position);
            });
        });
    });
});

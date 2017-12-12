/* global Ext, spyOn, jasmine, expect */

topSuite("Ext.Widget.floated", [false, 'Ext.Panel'], function() {
    var w;

    function makeWidget(cfg) {
        w = Ext.create(Ext.apply({
            hidden: false
        }, cfg));
        return w;
    }

    function expectXY(el, x, y) {
        if (el.isComponent) {
            el = el.element;
        }
        var box = Ext.fly(el).getBox();
        expect(box.x).toBe(x);
        expect(box.y).toBe(y);
    }

    afterEach(function() {
        var toDestroy = [Ext.Viewport, Ext.Widget.$mousedownListeners];

        if (w !== Ext.Viewport) {
            toDestroy.push(w);
        }

        w = Ext.Viewport = Ext.Widget.$mousedownListeners = Ext.destroy(toDestroy);

        // Restore body element to cleanliness after Viewport has mangled it.
        Ext.Object.eachValue(document.body.attributes, function(attr) {
            if (attr.name !== 'id') {
                document.body.removeAttribute(attr.name);
            }
        });
        document.body.style = '';
    });

    describe("Basic render on show", function() {
        it('should apply the correct CSS classes', function() {
            makeWidget({
                xtype: 'panel',
                border: true,
                title: 'Test',
                height: 100,
                width: 200,
                floated: true,
                shadow: true
            });
            w.show();

            expect(w.element.hasCls(w.floatedCls)).toBe(true);
            expect(w.element.hasCls(w.shadowCls)).toBe(true);
        });
        it('should apply the correct translation', function() {
            makeWidget({
                xtype: 'panel',
                border: true,
                title: 'Test',
                height: 100,
                width: 200,
                floated: true,
                x: 100,
                y: 200
            });
            w.show();
            expectXY(w, 100, 200);
        });
    });

    describe('converting from inner to floated', function() {
        it('should append the newly floated item to the closest float-root element', function() {
            w = new Ext.Container({
                xtype: 'container',
                height: 40,
                width: 600,
                items: {
                    itemId: 'item',
                    xtype: 'panel',
                    title: 'Inner Panel',
                    html: 'Inner Panel HTML'
                }
            });
            w.render(document.body);
            var item = w.child('#item');

            // Container's bodyElement contains the item
            expect(item.el.dom.parentNode === w.bodyElement.dom).toBe(true);

            item.setFloated(true);

            // The global float root contains the item encapsulated in its floatWrap
            expect(item.floatWrap.dom.parentNode === Ext.getFloatRoot().dom).toBe(true);

            // The global float root must always be on top
            expect(Ext.getFloatRoot().dom.nextSibling).toBe(null);

            item.setFloated(false);

            // Container's bodyElement must contain the item
            expect(item.el.dom.parentNode === w.bodyElement.dom).toBe(true);
        });
    });

    describe('hierarchical floateds', function() {
        var floatedPanel, floatedPanelChild, floatedPanelSecondChild,
            otherFloatedPanel, otherFloatedPanelChild, otherFloatedPanelSecondChild,
            wasRendered;

        beforeEach(function() {
            if (Ext.Msg) {
                wasRendered = Ext.Msg.rendered;
                // Tests assume a pristine floatRoot
                Ext.Msg.setRendered(false);
            }

            w = new Ext.viewport.Viewport.setup({
                items: [{
                    id: 'test-centered-panel',
                    xtype: 'panel',
                    floating: true,
                    title: 'Centered Panel',
                    height: 400,
                    width: 600,
                    border: true,
                    centered: true,
                    items: [{
                        xtype: 'fieldset',
                        title: 'Some fields',
                        items: [{
                            fieldLabel: 'Testing form fields',
                            xtype: 'textfield'
                        }]
                    }, {
                        xtype: 'panel',
                        height: 200,
                        width: 400,
                        border: true,
                        floated: true,
                        hidden: false,
                        id: 'floated-panel',
                        title: 'Floated panel',
                        x: 100,
                        y: 100,
                        shadow: true,
                        items: [{
                            border: true,
                            xtype: 'panel',
                            floated: true,
                            hidden: false,
                            id: 'floated-panel-child',
                            title: 'Floated panel child',
                            height: 100,
                            width: 300,
                            x: 150,
                            y: 150
                        }, {
                            border: true,
                            xtype: 'panel',
                            floated: true,
                            hidden: false,
                            id: 'floated-panel-second-child',
                            title: 'Floated panel second child',
                            height: 100,
                            width: 300,
                            x: 180,
                            y: 180
                        }]
                    }, {
                        xtype: 'panel',
                        height: 200,
                        width: 400,
                        border: true,
                        floated: true,
                        hidden: false,
                        relative: true,
                        id: 'other-floated-panel',
                        title: 'Other Floated panel',
                        x: 200,
                        y: 200,
                        shim: true,
                        items: [{
                            border: true,
                            xtype: 'panel',
                            floated: true,
                            hidden: false,
                            id: 'other-floated-panel-child',
                            title: 'Other floated panel child',
                            height: 100,
                            width: 300,
                            x: 50,
                            y: 50
                        }, {
                            border: true,
                            xtype: 'panel',
                            floated: true,
                            hidden: false,
                            id: 'other-floated-panel-second-child',
                            title: 'Other floated panel second child',
                            height: 100,
                            width: 300,
                            x: 80,
                            y: 80
                        }]
                    }]
                }]
            });
            floatedPanel = w.down('#floated-panel');

            // Extract the child floateds by looking in the floatWrap.
            // The child floated MUST be physically contained in the floatWrap in order to move
            // up or down the z-index stack in synchrony with their parent.
            floatedPanelChild = Ext.Component.from(floatedPanel.floatWrap.down('#floated-panel-child'), 1);
            floatedPanelSecondChild = Ext.Component.from(floatedPanel.floatWrap.down('#floated-panel-second-child'), 1);
            otherFloatedPanel = w.down('#other-floated-panel');
            otherFloatedPanelChild = w.down('#other-floated-panel-child');
            otherFloatedPanelSecondChild = w.down('#other-floated-panel-second-child');
        });

        afterEach(function() {
            if (wasRendered) {
                wasRendered = Ext.Msg.setRendered(false);
            }
            wasRendered = false;
        });

        it('should arrange the floated components in nested floatRoot elements', function() {
            var f;

            // Global float root should be immediately after the viewport body
            expect(Ext.Viewport.element.dom.nextSibling).toBe(Ext.floatRoot.dom);

            // Ensure correct start nesting positions.
            // Global float root contains the wrapping floatRoot elements of both global floateds
            // in initial order.
            f = Ext.floatRoot.dom.childNodes;
            expect(f[0]).toBe(floatedPanel.floatWrap.dom);
            expect(f[1]).toBe(otherFloatedPanel.floatWrap.dom);

            // The floated panel's floatRoot wrapper should always have the floated panel
            // as the first node, followed by the children in order
            expect(floatedPanel.floatWrap.dom.firstChild).toBe(floatedPanel.el.dom);
            expect(floatedPanel.floatWrap.dom.childNodes[1]).toBe(floatedPanelChild.floatWrap.dom);
            expect(floatedPanel.floatWrap.dom.childNodes[2]).toBe(floatedPanelSecondChild.floatWrap.dom);

            // The other floated panel's floatRoot wrapper should always have the other floated panel's shim
            // as the first node, then the other floated panel, followed by the children in order
            expect(otherFloatedPanel.floatWrap.dom.firstChild).toBe(otherFloatedPanel.getShim().el.dom);
            expect(otherFloatedPanel.floatWrap.dom.childNodes[1]).toBe(otherFloatedPanel.el.dom);
            expect(otherFloatedPanel.floatWrap.dom.childNodes[2]).toBe(otherFloatedPanelChild.floatWrap.dom);
            expect(otherFloatedPanel.floatWrap.dom.childNodes[3]).toBe(otherFloatedPanelSecondChild.floatWrap.dom);
        });

        it('should start with correct positions', function() {
            expectXY(floatedPanel, 100, 100);
            expectXY(floatedPanelChild, 150, 150);
            expectXY(floatedPanelSecondChild, 180, 180);
            expectXY(otherFloatedPanel, 200, 200);
            expectXY(otherFloatedPanel.getShim().el, 200, 200);
            expectXY(otherFloatedPanelChild, 250, 250);
            expectXY(otherFloatedPanelSecondChild, 280, 280);
        });

        it('should reset position on unfloat', function() {
            var otherFloatedPanelRegion = otherFloatedPanel.bodyElement.getConstrainRegion();

            // Unfloat the second chlid. This will insert it into the Other floated panel's layout
            // as the first inner item.
            otherFloatedPanelSecondChild.setFloated(false);

            // Should be the first inner item in Other floated panel, so will
            // be right at the top/left position
            expectXY(otherFloatedPanelSecondChild, otherFloatedPanelRegion.x, otherFloatedPanelRegion.y);
        });

        it('should move mousedowned floated components to above their siblings', function() {
            var r = floatedPanelChild.el.getRegion(),
                cfg = {
                    id: floatedPanelChild.id,
                    x: r.left + r.right / 2,
                    y: r.top + r.bottom / 2
                },
                toFrontDone;

            floatedPanelChild.on({
                tofront: function() {
                    toFrontDone = true;
                }
            });

            // Mousedown on the lowest in the hierarchy.
            // It should flip to the top.
            Ext.testHelper.touchStart(floatedPanelChild.el, cfg);
            Ext.testHelper.touchEnd(floatedPanelChild.el, cfg);

            // tofront event should have fired
            waitsFor(function() {
                return toFrontDone === true;
            });

            runs(function() {
                toFrontDone = false;

                // The two top level float roots should be swapped to bring floated panel
                // (along with its descendants) to the top
                expect(Ext.floatRoot.dom.firstChild).toBe(otherFloatedPanel.floatWrap.dom);
                expect(Ext.floatRoot.dom.childNodes[1]).toBe(floatedPanel.floatWrap.dom);

                // Floated panel should have had its child panels reorders
                // floatedPanelChild should be on top (last element)
                expect(floatedPanel.floatWrap.dom.firstChild).toBe(floatedPanel.el.dom);
                expect(floatedPanel.floatWrap.dom.childNodes[1]).toBe(floatedPanelSecondChild.floatWrap.dom);
                expect(floatedPanel.floatWrap.dom.childNodes[2]).toBe(floatedPanelChild.floatWrap.dom);

                // The inner details of Other floated panel must not have been affected
                expect(otherFloatedPanel.floatWrap.dom.firstChild).toBe(otherFloatedPanel.getShim().el.dom);
                expect(otherFloatedPanel.floatWrap.dom.childNodes[1]).toBe(otherFloatedPanel.el.dom);
                expect(otherFloatedPanel.floatWrap.dom.childNodes[2]).toBe(otherFloatedPanelChild.floatWrap.dom);
                expect(otherFloatedPanel.floatWrap.dom.childNodes[3]).toBe(otherFloatedPanelSecondChild.floatWrap.dom);
            });
        });

        it('should move a component to above its siblings on a toFront call', function() {
            var toFrontDone;

            floatedPanelChild.on({
                tofront: function() {
                    toFrontDone = true;
                }
            });

            // It should flip to the top.
            floatedPanelChild.toFront();

            // tofront event should have fired
            expect(toFrontDone).toBe(true);

            // The two top level float roots should be swapped to bring floated panel
            // (along with its descendants) to the top
            expect(Ext.floatRoot.dom.firstChild).toBe(otherFloatedPanel.floatWrap.dom);
            expect(Ext.floatRoot.dom.childNodes[1]).toBe(floatedPanel.floatWrap.dom);

            // Floated panel should have had its child panels reorders
            // floatedPanelChild should be on top (last element)
            expect(floatedPanel.floatWrap.dom.firstChild).toBe(floatedPanel.el.dom);
            expect(floatedPanel.floatWrap.dom.childNodes[1]).toBe(floatedPanelSecondChild.floatWrap.dom);
            expect(floatedPanel.floatWrap.dom.childNodes[2]).toBe(floatedPanelChild.floatWrap.dom);

            // The inner details of Other floated panel must not have been affected
            expect(otherFloatedPanel.floatWrap.dom.firstChild).toBe(otherFloatedPanel.getShim().el.dom);
            expect(otherFloatedPanel.floatWrap.dom.childNodes[1]).toBe(otherFloatedPanel.el.dom);
            expect(otherFloatedPanel.floatWrap.dom.childNodes[2]).toBe(otherFloatedPanelChild.floatWrap.dom);
            expect(otherFloatedPanel.floatWrap.dom.childNodes[3]).toBe(otherFloatedPanelSecondChild.floatWrap.dom);
        });

        it('should allow a toFront operation to be vetoed', function() {
            var toFrontDone = false;

            // veto toFront operation
            floatedPanelChild.on({
                beforetofront: function() {
                    return false;
                },
                tofront: function() {
                    toFrontDone = true;
                }
            });

            // This should not succeed
            floatedPanelChild.toFront();

            // tofront event should not have fired
            expect(toFrontDone).toBe(false);

            // Below is the same set of conditions as the "should arrange the floated components in nested floatRoot elements"
            // test above. Initial conditions should NOT have changed

            // Global float root should be immediately after the viewport body
            expect(Ext.Viewport.element.dom.nextSibling).toBe(Ext.floatRoot.dom);

            // Ensure correct start nesting positions.
            // Global float root contains the wrapping floatRoot elements of both global floateds
            // in initial order.
            expect(Ext.floatRoot.dom.firstChild).toBe(floatedPanel.floatWrap.dom);
            expect(Ext.floatRoot.dom.childNodes[1]).toBe(otherFloatedPanel.floatWrap.dom);

            // The floated panel's floatRoot wrapper should always have the floated panel
            // as the first node, followed by the children in order
            expect(floatedPanel.floatWrap.dom.firstChild).toBe(floatedPanel.el.dom);
            expect(floatedPanel.floatWrap.dom.childNodes[1]).toBe(floatedPanelChild.floatWrap.dom);
            expect(floatedPanel.floatWrap.dom.childNodes[2]).toBe(floatedPanelSecondChild.floatWrap.dom);

            // The other floated panel's floatRoot wrapper should always have the other floated panel's shim
            // as the first node, then the other floated panel, followed by the children in order
            expect(otherFloatedPanel.floatWrap.dom.firstChild).toBe(otherFloatedPanel.getShim().el.dom);
            expect(otherFloatedPanel.floatWrap.dom.childNodes[1]).toBe(otherFloatedPanel.el.dom);
            expect(otherFloatedPanel.floatWrap.dom.childNodes[2]).toBe(otherFloatedPanelChild.floatWrap.dom);
            expect(otherFloatedPanel.floatWrap.dom.childNodes[3]).toBe(otherFloatedPanelSecondChild.floatWrap.dom);
        });

        it('should give alwaysOnTop floated components a higher z-index', function() {
            // Initial conditions
            expect(otherFloatedPanel.floatWrap.dom.firstChild).toBe(otherFloatedPanel.getShim().el.dom);
            expect(otherFloatedPanel.floatWrap.dom.childNodes[1]).toBe(otherFloatedPanel.el.dom);
            expect(otherFloatedPanel.floatWrap.dom.childNodes[2]).toBe(otherFloatedPanelChild.floatWrap.dom);
            expect(otherFloatedPanel.floatWrap.dom.childNodes[3]).toBe(otherFloatedPanelSecondChild.floatWrap.dom);

            otherFloatedPanelChild.setAlwaysOnTop(true);

            // otherFloatedPanelChild must be the last element in the stack
            expect(otherFloatedPanel.floatWrap.dom.firstChild).toBe(otherFloatedPanel.getShim().el.dom);
            expect(otherFloatedPanel.floatWrap.dom.childNodes[1]).toBe(otherFloatedPanel.el.dom);
            expect(otherFloatedPanel.floatWrap.dom.childNodes[2]).toBe(otherFloatedPanelSecondChild.floatWrap.dom);
            expect(otherFloatedPanel.floatWrap.dom.childNodes[3]).toBe(otherFloatedPanelChild.floatWrap.dom);

            // This should have no effect
            otherFloatedPanelSecondChild.toFront();

            // otherFloatedPanelChild must STILL be the last element in the stack
            expect(otherFloatedPanel.floatWrap.dom.firstChild).toBe(otherFloatedPanel.getShim().el.dom);
            expect(otherFloatedPanel.floatWrap.dom.childNodes[1]).toBe(otherFloatedPanel.el.dom);
            expect(otherFloatedPanel.floatWrap.dom.childNodes[2]).toBe(otherFloatedPanelSecondChild.floatWrap.dom);
            expect(otherFloatedPanel.floatWrap.dom.childNodes[3]).toBe(otherFloatedPanelChild.floatWrap.dom);
        });

        it('should destroy correctly, removing all traces from the DOM', function() {
            floatedPanel.destroy();
            otherFloatedPanel.destroy();

            // Nothing in the floatRoot now.
            expect(Ext.floatRoot.dom.childNodes.length).toBe(0);
        });

        it('should size the modal mask correctly, and move it to below the topmost floated when floated set to false', function() {
            var mask;

            otherFloatedPanelSecondChild.setModal(true);
            mask = otherFloatedPanelSecondChild.floatParentNode.getData().modalMask;

            // The mask must be immediately before the panel in the DOM
            expect(mask.dom.nextSibling).toBe(otherFloatedPanelSecondChild.floatWrap.dom);
            expect(mask.getSize()).toEqual(otherFloatedPanelSecondChild.parent.el.getSize());

            otherFloatedPanelChild.toFront();
            otherFloatedPanelChild.setModal(true);

            // The mask must be immediately before the panel in the DOM
            expect(mask.dom.nextSibling).toBe(otherFloatedPanelChild.floatWrap.dom);
            expect(mask.getSize()).toEqual(otherFloatedPanelChild.parent.el.getSize());

            // The mask must NOT be immediately before the panel in the DOM
            otherFloatedPanelChild.setModal(false);
            expect(otherFloatedPanelChild.floatWrap.dom.previousSibling).not.toBe(mask.dom);

            // The mask must have dropped to just below the other, lower modal
            expect(otherFloatedPanelSecondChild.floatWrap.dom.previousSibling).toBe(mask.dom);

            // Make it so that there are NO visible modals.
            otherFloatedPanelSecondChild.setModal(false);

            // So the mask ust be stashed safely in the detached body
            expect(mask.dom.parentNode).toBe(Ext.getDetachedBody().dom);
        });

        it('should size the modal mask correctly, and move it to below the topmost floated when hidden - relative', function() {
            var mask;

            otherFloatedPanelSecondChild.setModal(true);
            mask = otherFloatedPanelSecondChild.floatParentNode.getData().modalMask;

            // The mask must be immediately before the panel in the DOM
            expect(mask.dom.nextSibling).toBe(otherFloatedPanelSecondChild.floatWrap.dom);
            expect(mask.getSize()).toEqual(otherFloatedPanelSecondChild.parent.el.getSize());

            otherFloatedPanelChild.toFront();
            otherFloatedPanelChild.setModal(true);

            // The mask must be immediately before the panel in the DOM
            expect(mask.dom.nextSibling).toBe(otherFloatedPanelChild.floatWrap.dom);
            expect(mask.getSize()).toEqual(otherFloatedPanelChild.parent.el.getSize());

            // Hide topmost modal
            otherFloatedPanelChild.hide(false);

            // The mask must have dropped to just below the other, lower modal
            expect(otherFloatedPanelSecondChild.floatWrap.dom.previousSibling).toBe(mask.dom);

            // Make it so that there are NO visible modals.
            otherFloatedPanelSecondChild.setModal(false);

            // So the mask ust be stashed safely in the detached body
            expect(mask.dom.parentNode).toBe(Ext.getDetachedBody().dom);
        });

        it('should move the mask to below the topmost floated when hidden - absolute', function() {
            var floatRoot = Ext.getFloatRoot(),
                mask;

            floatedPanel.toFront();

            floatedPanelSecondChild.setModal(true);
            mask = floatRoot.getData().modalMask;

            // The mask must be immediately before the panel in the DOM
            expect(mask.dom.nextSibling).toBe(floatedPanelSecondChild.floatWrap.dom);
            expect(mask.getSize()).toEqual(floatRoot.getSize());

            floatedPanelChild.toFront();
            floatedPanelChild.setModal(true);

            // The mask must be immediately before the panel in the DOM
            expect(mask.dom.nextSibling).toBe(floatedPanelChild.floatWrap.dom);
            expect(mask.getSize()).toEqual(floatRoot.getSize());

            // Hide topmost modal
            floatedPanelChild.hide(false);

            // The mask must have dropped to just below the other, lower modal
            expect(floatedPanelSecondChild.floatWrap.dom.previousSibling).toBe(mask.dom);

            // Make it so that there are NO visible modals.
            floatedPanelSecondChild.setModal(false);

            // So the mask ust be stashed safely in the detached body
            expect(mask.dom.parentNode).toBe(Ext.getDetachedBody().dom);
        });
    });

    // Phones do not use floated pickers
    if (!Ext.platformTags.phone) {
        describe('non-parent hierarchy', function () {
            var dialog, dateField, picker;

            afterEach(function () {
                Ext.destroy(dialog);
            });

            it('should render the float roots of non-child descendant floateds into the closest ancestor floatWrap', function () {
                dialog = new Ext.Dialog({
                    modal: false,
                    title: 'Floated Panel',
                    height: 400,
                    width: 600,
                    items: [{
                        xtype: 'datefield',
                        id: 'test-datefield'
                    }]
                });
                dialog.showAt(0, 0);
                dateField = Ext.getCmp('test-datefield');
                dateField.expand();
                picker = dateField.getPicker();

                // The dialog's floatWrap must contain the floated picker's floatWrap
                // So that they move up and down the z-index hierarchy together when
                // the dialog moves up or down.
                expect(dialog.floatWrap.contains(picker.floatWrap)).toBe(true);

                dialog.hide();
            });
        });
    }
});
/**
 * Prevents DOM focusability while modal windows are visible.
 */
Ext.define('Ext.plugin.TabGuard', {
    extend: 'Ext.plugin.Abstract',
    alias: 'plugin.tabguard',
    
    /**
     * When set to `true`, two elements are added to the container's element. These are the
     * `{@link #tabGuardBeforeEl}` and `{@link #tabGuardAfterEl}`.
     * @cfg {Boolean} tabGuard
     * @private
     * @since 6.5.1
     */
    tabGuard: true,
    
    /**
     * @cfg {Number} [tabGuardBeforeIndex] Top tab guard tabIndex. Use this when
     * there are elements with tabIndex > 0 within the dialog.
     * @private
     * @since 6.5.1
     */
    
    /**
     * @cfg {Number} [tabGuardAfterIndex] Bottom tab guard tabIndex. Use this
     * when there are elements with tabIndex > 0 within the dialog.
     * @private
     * @since 6.5.1
     */

    /**
     * @property {Array} tabGuardTemplate
     * This template is used to generate the `tabGuard` elements. It is used once per
     * element (see `{@link #tabGuardBeforeEl}` and `{@link #tabGuardAfterEl}`).
     * @private
     * @since 6.5.1
     */
    tabGuardTemplate: [{
        // We use span instead of div because of IE bug/misfeature: it will focus
        // block elements upon clicking or calling node.focus() regardless of
        // tabIndex attribute. It doesn't do that with inline elements, hence span.
        tag: 'span',
        'aria-hidden': 'true',
        cls: Ext.baseCSSPrefix + 'tab-guard-el'
    }],
    
    /**
     * @property {Object} tabGuardElements
     * Read only object containing property names for tab guard elements, keyed by position.
     * @private
     * @since 6.5.1
     */
    tabGuardElements: {
        before: 'tabGuardBeforeEl',
        after: 'tabGuardAfterEl'
    },
    
    init: function(cmp) {
        var me = this;
        
        me.decorateComponent(cmp);
        
        if (cmp.addTool) {
            cmp.addTool = Ext.Function.createSequence(cmp.addTool, me.maybeInitTabGuards, me);
        }
        
        if (cmp.add) {
            cmp.add = Ext.Function.createSequence(cmp.add, me.maybeInitTabGuards, me);
        }
        
        if (cmp.remove) {
            cmp.remove = Ext.Function.createSequence(cmp.remove, me.maybeInitTabGuards, me);
        }
        
        cmp.getTabGuard = me.getTabGuard.bind(me);
        
        cmp.on('show', me.initTabGuards, me);
    },
    
    destroy: function() {
        var cmp = this.getCmp();
        
        if (cmp) {
            delete cmp.addTool;
            delete cmp.add;
            delete cmp.remove;
        }
        
        this.callParent();
    },
    
    privates: {
        decorateComponent: function(cmp) {
            var tpl = this.tabGuardTemplate;
            
            cmp = cmp || this.getCmp();
            
            cmp[this.tabGuardElements.before] = cmp.el.insertFirst(tpl);
            cmp[this.tabGuardElements.after] = cmp.el.createChild(tpl);
        },
        
        getTabGuard: function(position) {
            var cmp = this.getCmp(),
                prop = this.tabGuardElements[position];
            
            return cmp[prop];
        },
        
        maybeInitTabGuards: function() {
            var cmp = this.getCmp();
            
            if (cmp.rendered && cmp.initialized && cmp.tabGuard) {
                this.initTabGuards();
            }
        },
        
        initTabGuards: function() {
            var me = this,
                cmp = me.getCmp(),
                minTabIndex = me.tabGuardBeforeIndex || 0,
                maxTabIndex = me.tabGuardAfterIndex || 0,
                beforeGuard = me.getTabGuard('before'),
                afterGuard = me.getTabGuard('after'),
                i, tabIndex, nodes;
            
            if (!cmp.rendered || !cmp.tabGuard) {
                return;
            }
            
            nodes = cmp.el.findTabbableElements({
                skipSelf: true
            });
            
            // Both tab guards may be in the list, disregard them
            if (nodes[0] === beforeGuard.dom) {
                nodes.shift();
            }
            
            if (nodes[nodes.length - 1] === afterGuard.dom) {
                nodes.pop();
            }
            
            if (nodes && nodes.length) {
                // In some cases it might be desirable to configure before and after
                // guard elements' tabIndex explicitly but if it is missing we try to
                // infer it from the DOM. If we don't and there are elements with
                // tabIndex > 0 within the container then tab order will be very
                // unintuitive.
                if (minTabIndex == null || maxTabIndex == null) {
                    for (i = 0; i < nodes.length; i++) {
                        // Can't use node.tabIndex property here
                        tabIndex = +nodes[i].getAttribute('tabIndex');
                        
                        if (tabIndex > 0) {
                            minTabIndex = Math.min(minTabIndex, tabIndex);
                            maxTabIndex = Math.max(maxTabIndex, tabIndex);
                        }
                    }
                }
                
                beforeGuard.dom.setAttribute('tabIndex', minTabIndex);
                afterGuard.dom.setAttribute('tabIndex', maxTabIndex);
            }
            else {
                // We don't want the guards to participate in tab flow
                // if there are no tabbable children in the container
                beforeGuard.dom.removeAttribute('tabIndex');
                afterGuard.dom.removeAttribute('tabIndex');
            }
            
            if (!beforeGuard.hasListeners.focusenter) {
                beforeGuard.on('focusenter', me.onTabGuardFocusEnter, cmp);
            }
            
            if (!afterGuard.hasListeners.focusenter) {
                afterGuard.on('focusenter',  me.onTabGuardFocusEnter, cmp);
            }
        },
        
        onTabGuardFocusEnter: function(e, target) {
            var cmp = this,
                el = cmp.el,
                beforeGuard = cmp.getTabGuard('before'),
                afterGuard = cmp.getTabGuard('after'),
                from = e.relatedTarget,
                nodes, forward, nextFocus;

            nodes = el.findTabbableElements({
                skipSelf: true
            });
            
            // Tabbables might include two tab guards, so remove them
            if (nodes[0] === beforeGuard.dom) {
                nodes.shift();
            }
            
            if (nodes[nodes.length - 1] === afterGuard.dom) {
                nodes.pop();
            }
            
            // Totally possible not to have anything tabbable within the window
            // but we have to do something so focus back the window el. At least
            // in that case the user will be able to press Escape key to close it.
            if (nodes.length === 0) {
                nextFocus = el;
            }
            // The window itself was focused, possibly by clicking or programmatically;
            // but this time we do have something tabbable to choose from.
            else if (from === el.dom) {
                forward = target === beforeGuard.dom;
            }
            // Focus was within the window and is trying to escape; 
            // for topmost guard we need to bounce focus back to the last tabbable
            // element in the window, and vice versa for the bottom guard.
            else if (el.contains(from)) {
                forward = !!e.forwardTab;
            }
            // It is entirely possible that focus was outside the window and
            // the user tabbed into the window. In that case we forward the focus
            // to the next available element in the natural tab order, i.e. the element
            // after the topmost guard, or the element before the bottom guard.
            else {
                forward = target === beforeGuard.dom;
            }
            
            nextFocus = nextFocus || (forward ? nodes[0] : nodes[nodes.length - 1]);
            
            if (nextFocus) {
                // If there is only one focusable node in the window, focusing it
                // while we're in focusenter handler for the tab guard might cause
                // race condition where the focusable node will be refocused first
                // and then its original blur handler will kick in, removing focus
                // styling erroneously.
                Ext.fly(nextFocus).focus(nodes.length === 1 ? 1 : 0);
            }
        }
    }
});

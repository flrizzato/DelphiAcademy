/**
 * A Toolable component is a lightweight container of `Ext.Tool` components. The tools
 * are defined using the `tools` config like so:
 *
 *      tools: [{
 *          type: 'gear',
 *          itemId: 'settings'
 *      }]
 *
 * Equivalently tools can also be declared as a keyed container of `itemId`s:
 *
 *      tools: {
 *          settings: {
 *              type: 'gear'
 *          }
 *      }
 *
 * This second form is more flexible since it allow config system merging. Unfortunately
 * this form does not provide the same simplicity for controlling tool order. To control
 * item order the tools can be assigned a {@link Ext.Component#cfg!weight}.
 *
 * Consider this array form:
 *
 *      tools: [{
 *          type: 'gear',
 *          itemId: 'settings'
 *      }, {
 *          type: 'pin',
 *          itemId: 'pin
 *      }]
 *
 * The equivalent object form would be:
 *
 *      tools: {
 *          settings: {
 *              type: 'gear',
 *              weight: 1
 *          },
 *          pin: {
 *              type: 'pin',
 *              weight: 2
 *          }
 *      }
 *
 * @private
 * @since 6.5.0
 */
Ext.define('Ext.mixin.Toolable', {
    mixinId: 'toolable',

    config: {
        /**
         * @cfg {Object} defaultToolWeights
         * The default `weight` for tools in the `header`.
         * @since 6.5.0
         */
        defaultToolWeights: {
            cached: true,
            $value: {
                toggle: 10,
                gear: 20,

                prev: 30,
                next: 40,

                left: 50,
                right: 60,
                down: 70,
                up: 80,

                refresh: 90,
                disclosure: 100, // was originally defined in ListItem
                plus: 100,
                minus: 110,
                search: 120,
                save: 130,
                print: 140,

                expand: 150,
                collapse: 160,
                help: 170,
                pin: 180,
                unpin: 190,

                minimize: 200,
                maximize: 210,
                restore: 220,
                close: 230
            }
        },

        /**
         * @cfg {Object} toolDefaults
         * The properties of this object are shallow copied (via {@link Ext#applyIf applyIf()}
         * as opposed to {@link Ext#merge Ext.merge()} to each tool declared in the `tools`
         * config.
         */
        toolDefaults: {
            xtype: 'tool',
            zone: 'end'
        },

        // @cmd-auto-dependency {aliasPrefix: "widget.", defaultType: 'Ext.Tool', typeProperty: "xtype", defaultsProperty: "toolDefaults", isKeyedObject: true}
        /**
         * @cfg {Ext.Tool[]/Object/Object[]} tools
         * An array of {@link Ext.Tool} configs or an object keyed by `itemId`.
         */
        tools: null
    },

    /**
     * @private
     * The name of the reference element to use as the "anchor" for the tool zones.
     * The start zone is inserted just prior to the anchor element and the tail and end
     * zones are inserted immediately after.
     *
     * Not applicable for Ext.Container instances as they use docked items to create
     * the tool zones
     */
    toolAnchorName: 'bodyElement',

    afterClassMixedIn: function (targetClass) {
        var proto = targetClass.prototype,
            already = proto.toolDefaults,
            getRefItems = proto.getRefItems;

        if (already) {
            delete proto.toolDefaults;

            targetClass.getConfigurator().add({
                toolDefaults: Ext.apply({
                    xtype: 'tool',
                    weight: 0,
                    zone: 'end'
                }, already)
            });
        }

        already = proto.tools;
        if (already) {
            delete proto.tools;

            targetClass.getConfigurator().add({
                tools: already
            });
        }

        // We are being mixed into a component which has a getRefItems implementation.
        // getRefItems needs to be augmented to also return the Tools
        if (getRefItems) {
            proto.getRefItems = function(deep) {
                return Ext.Array.push(getRefItems.call(this, deep), this.getTools() || Ext.emptyArray);
            };
        }
        // Not a container - return getTools results;
        else {
            proto.getRefItems = function() {
                return this.getTools() || Ext.emptyArray;
            };
        }
    },

    lookupTool: function (id) {
        var tools = this.getTools(),
            n = tools && tools.length,
            i, tool;

        for (i = 0; i < n; ++i) {
            tool = tools[i];

            if (tool.type === id || tool.getItemId() === id) {
                return tool;
            }
        }

        return null;
    },

    // tools

    applyTools: function (tools) {
        if (tools) {
            var me = this,
                array = me.createTools(tools),
                n = array.length,
                i, tool, zone;

            Ext.Array.sort(array, Ext.weightSortFn);

            for (i = 0; i < n; ++i) {
                tool = array[i];
                tool.ownerCmp = tool.toolOwner = me;

                array[i] = tool = Ext.create(tool);

                tool.doInheritUi();

                zone = tool.zone;

                tool.addCls(me._toolPositionClsMap[zone]);
                me.getToolZone(tool.zone).el.appendChild(tool.el);
            }

            tools = array;
        }

        return tools;
    },

    updateTools: function (tools, oldTools) {
        Ext.destroy(oldTools);
    },

    privates: {
        _toolZoneNames: {
            end: '_endZone',
            head: '_headZone',
            start: '_startZone',
            tail: '_tailZone'
        },

        _tailedCls: Ext.baseCSSPrefix + 'tailed',
        _headedCls: Ext.baseCSSPrefix + 'headed',
        _toolZoneCls: Ext.baseCSSPrefix + 'tool-zone',

        // These classes are added to the tool zone elements or components
        _toolZoneClsMap: {
            end: Ext.baseCSSPrefix + 'end',
            head: Ext.baseCSSPrefix + 'head',
            tail: Ext.baseCSSPrefix + 'tail',
            start: Ext.baseCSSPrefix + 'start'
        },

        // These classes are added to the tool instances themselves.  They are used by
        // the tool-ui mixin to add margin to one side of the tool.  Tools in the "tail"
        // zone just get the "end" cls because the margin is on the same side for both zones.
        // Panel headers also use these class names on their tools - tools that come
        // before the title get the x-start cls and tools that are positioned after the
        // title get the x-end cls.
        // This allows the tool-ui mixin to use one simple selector to style the tool
        // margins regardless of how the tool is created or contained.
        _toolPositionClsMap: {
            end: Ext.baseCSSPrefix + 'end',
            head: Ext.baseCSSPrefix + 'start', // head == start for margin purposes
            tail: Ext.baseCSSPrefix + 'end',   // tail == end for margin purposes
            start: Ext.baseCSSPrefix + 'start'
        },

        _toolDockAlignCls: {
            left: Ext.baseCSSPrefix + 'align-left',
            center: Ext.baseCSSPrefix + 'align-center',
            right: Ext.baseCSSPrefix + 'align-right'
        },

        hasToolZones: false,

        adjustToolDefaults: function (tool, toolDefaults, defaultToolWeights) {
            toolDefaults = toolDefaults || this.getToolDefaults();

            if (defaultToolWeights === undefined) {
                defaultToolWeights = this.getDefaultToolWeights();
            }

            if (toolDefaults) {
                Ext.applyIf(tool, toolDefaults);
                
                tool.instanceCls = this.toolCls;
            }

            if (!tool.type && !tool.iconCls) {
                tool.type = tool.itemId;
            }

            if (defaultToolWeights && !('weight' in tool)) {
                tool.weight = defaultToolWeights[tool.type];
            }

            return tool;
        },

        createTools: function (tools, toolOwner) {
            var me = this,
                array = Ext.convertKeyedItems(tools, 'handler', 'handler'),
                n = array.length,
                defaultToolWeights = me.getDefaultToolWeights(),
                toolDefaults = me.getToolDefaults(),
                i, tool;

            toolOwner = toolOwner || me;

            if (array === tools) { // if (wasn't an object)
                array = [];

                for (i = 0; i < n; ++i) {
                    tool = tools[i];

                    if (typeof tool === 'string') {
                        tool = me.adjustToolDefaults({ type: tool }, toolDefaults, null);
                    }
                    else {
                        tool = Ext.apply(me.adjustToolDefaults({}, toolDefaults, null), tool);
                    }

                    tool.toolOwner = toolOwner;

                    array[i] = tool;
                }
            }
            else {
                // convertKeyedItems has already shallow copied each item in order
                // to place in the itemId, so leverage that... It has also promoted
                // string items like 'foo' in to objects like { type: 'foo' }.
                for (i = 0; i < n; ++i) {
                    me.adjustToolDefaults(tool = array[i], toolDefaults, defaultToolWeights);

                    tool.toolOwner = toolOwner;
                }
            }

            return array;
        },

        getToolZone: function (zoneName) {
            var me = this,
                zonePropName = me._toolZoneNames[zoneName],
                zone = me[zonePropName],
                dockWrapName = '_toolDockWrap',
                anchorElement;

            //<debug>
            if (!zonePropName) {
                Ext.raise('Invalid zone name: "' + zoneName + '"');
            }
            //</debug>

            if (!zone) {
                zone = Ext.Element.create({
                    classList: [me._toolZoneCls, me._toolZoneClsMap[zoneName]]
                });

                anchorElement = me[me.toolAnchorName];

                //<debug>
                if (!anchorElement) {
                    Ext.raise('Invalid tool anchor. No element named "' + me.toolAnchorName + '".');
                }
                //</debug>

                // The toolDockWrap is an element that wraps the tool zones and the
                // tool anchor (the element to which the tool zones are anchored on either side)
                // At the styling level it behaves just like the dock wrapper created by auto
                // layout in a container that has docked items.
                if (!me[dockWrapName]) {
                    me[dockWrapName] = anchorElement.wrap({
                        cls: Ext.baseCSSPrefix + 'tool-dock'
                    });

                    anchorElement.addCls(Ext.baseCSSPrefix + 'tool-anchor');

                    // The stylesheet needs to move the horizontal body padding onto the
                    // tool dock wrapper.  In order for the UI mixins to accomplish this
                    // We must add the dock wrapper to the list of elements that have
                    // UI and xtype info munged into their class name
                    me.initUiReference(dockWrapName, 'tool-dock');
                    me.syncToolableAlign();
                }

                if (zoneName === 'head') {
                    zone.insertBefore(anchorElement);
                    anchorElement.addCls(me._headedCls);
                } else if (zoneName === 'tail') {
                    zone.insertAfter(anchorElement);
                    anchorElement.addCls(me._tailedCls);
                } else if (zoneName === 'start') {
                    zone.insertBefore(me._headZone || anchorElement);
                } else if (zoneName === 'end') {
                    zone.insertAfter(me._tailZone || anchorElement);
                }

                me[zonePropName] = zone;

                me.hasToolZones = true;
            }

            return zone;
        },

        /**
         * @private
         * Synchronizes an alignment cls on the tool dock wrapper when the alignment changes.
         * Only applicable for toolable components that have an `align` config such as
         * grid cells and column headers
         */
        syncToolableAlign: function() {
            var me = this,
                dockWrap = me._toolDockWrap,
                alignCls = me._toolDockAlignCls,
                align;
            
            if (dockWrap && (typeof me.getAlign === 'function')) {
                align = me.getAlign();
                dockWrap.replaceCls(alignCls[me._toolDockAlign], alignCls[align]);
                me._toolDockAlign = align;
            }
        },

        doDestroy: function() {
            var me = this;

            me.setTools(null);

            Ext.destroy(me._startZone, me._endZone, me._headZone, me._tailZone, me._toolDockWrap);
        }
    }
});

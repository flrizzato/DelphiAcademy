/**
 * This class allows a {@link Ext.Panel Panel} to be resized via user interaction.
 * It can be used on floating panels, or as a splitter between two panels.
 *
 * @since 6.5.0
 */
Ext.define('Ext.panel.Resizer', {
    requires: ['Ext.panel.Resizable'],

    config: {
        /**
         * @cfg {Boolean} constrainToParent
         * `true` to constrain the dragging operation to the parent
         * of the {@link #cfg-target}.
         */
        constrainToParent: true,

        /**
         * @cfg {Boolean} dynamic
         * `true` to live resize the {@link #cfg-target}. `false` to create
         * a proxy indicator to represent the drag operation.
         */
        dynamic: false,

        /**
         * @cfg {String/String[]} edges
         * The draggable edges. These can be specified as a string separated by ' ' or ','. The
         * values for the edges should be direction coordinates (or the shortcut). Possible values are:
         *
         * - `'north'`, or `'n'`
         * - `'northeast'`, or `'ne'`
         * - `'east'`, or `'e'`
         * - `'southeast'`, or `'se'`
         * - `'south'`, or `'s'`
         * - `'southwest'`, or `'sw'`
         * - `'west'`, or `'w'`
         * - `'northwest'`, or `'nw'`
         * - `'all'`, a shortcut for all edges
         *
         * Examples:
         * - `['n', 'e', 's', 'w']`,
         * - `'e,se,s'`
         * - `'e se s'`,
         * - `'northeast southeast southwest northwest'`
         */
        edges: null,

        /**
         * @cfg {Number/Number[]} maxSize
         * The maximum width and height for this resizer. If specified as a number,
         * the value applies for both width and height. Otherwise,
         * - `[100, null]`, constrain only the width
         * - `[null, 100]`, constrain only the height
         * - `[200, 300]`, constrain both.
         *
         * **Note** If a {@link Ext.Component#cfg-maxWidth maxWidth} or {@link Ext.Component#cfg-maxHeight maxHeight}
         * is specified, it will take precedence.
         */
        maxSize: null,

        /**
         * @cfg {Number/Number[]} minSize
         * The minimum width and height for this resizer. If specified as a number,
         * the value applies for both width and height. Otherwise,
         * - `[100, null]`, constrain only the width
         * - `[null, 100]`, constrain only the height
         * - `[200, 300]`, constrain both.
         *
         * **Note** If a {@link Ext.Component#cfg-minWidth minWidth} or {@link Ext.Component#cfg-minHeight minHeight}
         * is specified, it will take precedence.
         */
        minSize: null,

        /**
         * {@cfg} {Boolean} preserveRatio
         * `true` to preserve the current aspect ratio of the component while dragging.
         */
        preserveRatio: false,

        /**
         * @cfg {Number/Number[]} snap
         * The interval to move during a resize. If specified as a number,
         * the value applies for both width and height. Otherwise,
         * - `[100, null]`, snap only the width
         * - `[null, 100]`, snap only the height
         * - `[200, 300]`, snap both.
         */
        snap: null,

        /**
         * @cfg {Boolean} split
         * `true` to operate in splitter mode, which is typically used for {@link Ext.Component#cfg-docked docked} items or
         * items in a {@link Ext.layout.Box box layout}. `false` to operate in handle mode, which is often used for floating
         * items.
         */
        split: false,

        /**
         * @cfg {Ext.Panel} target
         * The panel to be resized.
         *
         * @readonly
         */
        target: null,

        /**
         * @cfg {String} ui
         * The ui, inherited by the panel
         *
         * @private
         */
        ui: null
    },

    constructor: function(config) {
        this.edgeMap = {};
        this.initConfig(config);
    },

    applyEdges: function(edges, oldEdges) {
        var positionMap = this.positionMap,
            len, i;

        if (edges) {
            if (edges === 'all') {
                edges = Ext.Array.clone(this.allEdges);
            } else if (typeof edges === 'string') {
                edges = edges.trim();
                edges = edges.split(this.edgeDelimiterRe);
            } else {
                edges = Ext.Array.clone(edges);
            }

            for (i = 0, len = edges.length; i < len; ++i) {
                //<debug>
                if (!positionMap[edges[i]]) {
                    Ext.raise('Invalid edge specified: ' + edges[i]);
                }
                //</debug>
                edges[i] = positionMap[edges[i]];
            }

            edges.sort();
        }

        // If we have oldEdges, it must be sorted
        if (edges && oldEdges && Ext.Array.equals(edges, oldEdges)) {
            edges = undefined;
        }

        return edges;
    },

    updateEdges: function(edges, oldEdges) {
        var me = this,
            map = me.edgeMap,
            newMap = {},
            split = me.getSplit(),
            baseCls = me.baseCls,
            infoMap = me.edgeInfoMap,
            ui = me.getUi(),
            splitEdges = me.splitEdges,
            disabled = me.disabled,
            firstCorner = me.firstCorner,
            el = me.getTarget().element,
            splitPrefix = me.splitPrefix,
            edgeEl, key, len, i, edge;

        if (edges) {
            for (i = 0, len = edges.length; i < len; ++i) {
                newMap[edges[i]] = true;
            }
        }

        if (oldEdges) {
            for (key in map) {
                if (!newMap[key]) {
                    edgeEl = map[key];
                    if (infoMap[key].corner && edgeEl === firstCorner) {
                        firstCorner = null;
                    }
                    edgeEl.destroy();
                    if (split && key in splitEdges) {
                        el.removeCls(splitPrefix + key);
                    }
                }
            }

            // This means we just destroyed the firstCorner, update it if possible.
            if (me.firstCorner && !firstCorner) {
                me.firstCorner = el.child('.' + this.baseCls + '[data-corner]');
            }
        }

        if (edges) {
            for (key in newMap) {
                newMap[key] = edge = map[key] || me.createEdge(el, key);
                if (split) {
                    edge.addCls(me.splitterCls);

                    if (key in splitEdges) {
                        el.addCls(splitPrefix + key);
                    }
                } else {
                    edge.addCls(me.handleCls);
                }

                if (disabled) {
                    edge.addCls(me.disabledCls);
                }

                if (ui) {
                    edge.addCls(ui, baseCls);
                }
            }
        }

        this.edgeMap = newMap;
    },

    applyMaxSize: function(maxSize) {
        return this.applyConstraintValue(maxSize);
    },

    applyMinSize: function(minSize) {
        return this.applyConstraintValue(minSize);
    },

    applySnap: function(snap) {
        return this.applyConstraintValue(snap);
    },

    updateSplit: function(split) {
        var me = this,
            map = me.edgeMap,
            splitEdges = me.splitEdges,
            splitPrefix = me.splitPrefix,
            key, el, i, len, target;

        if (me.isConfiguring) {
            // Drop out if we're configuring, this the class manipulation
            // will happen during edge creation
            return;
        }

        target = me.getTarget().element;

        for (key in map) {
            el = map[key];
            if (el.isElement) {
                el.toggleCls(me.splitterCls, split);
                el.toggleCls(me.handleCls, !split);

                if (key in splitEdges) {
                    target.toggleCls(splitPrefix + key, split);
                }
            }
        }
    },

    updateTarget: function(target) {
        var me = this;

        me.targetListeners = me.dragListeners = Ext.destroy(me.targetListeners, me.dragListeners);

        if (target) {
            me.setupDragListeners();

            me.targetListeners = target.on({
                expand: 'onTargetExpand',
                collapse: 'onTargetCollapse',
                scope: me,
                destroyable: true
            });

            if (target.hasCollapsible && target.getCollapsed()) {
                me.onTargetCollapse(target);
            }
        }
    },

    updateUi: function(ui, oldUi) {
        var edgeMap = this.edgeMap,
            baseCls = this.baseCls,
            key, edge;

        if (this.isConfiguring) {
            return;
        }

        for (key in edgeMap) {
            edge = edgeMap[key];

            if (oldUi) {
                edge.removeCls(oldUi, baseCls);
            }

            if (ui) {
                edge.addCls(ui, baseCls);
            }
        }
    },

    destroy: function() {
        var me = this,
            target = me.getTarget();

        if (me.dragStarted) {
            me.cleanup();   
        }

        // If the target is destroying it will destroy the edge elements
        if (target && !target.destroying) {
            me.setEdges(null);
        }

        me.setTarget(null);
        me.callParent();
    },

    privates: {
        baseCls: Ext.baseCSSPrefix + 'panelresizer',
        proxyCls: Ext.baseCSSPrefix + 'panelresizer-proxy',

        disabledCls: Ext.baseCSSPrefix + 'disabled',

        handleCls: Ext.baseCSSPrefix + 'handle',
        splitterCls: Ext.baseCSSPrefix + 'splitter',
        horizontalCls: Ext.baseCSSPrefix + 'horizontal',
        verticalCls: Ext.baseCSSPrefix + 'vertical',
        splitPrefix: Ext.baseCSSPrefix + 'split-',

        edgeDelegateSelector: '> .' + Ext.baseCSSPrefix + 'panelresizer',

        /**
         * @property {String[]} allEdges
         * A shortcut to provide all the edges.
         *
         * @private
         */
        allEdges: [
            'north', 
            'northeast', 
            'east', 
            'southeast',
            'south',
            'southwest',
            'west',
            'northwest'
        ],

        /**
         * @property {Number} defaultMaxSize
         * A maximum size to use for constraining if it isn't configured
         * on either the component or this resizer.
         *
         * @private
         */
        defaultMaxSize: 100000,

        /**
         * @property {Number} defaultMaxSize
         * A minimum size to use for constraining if it isn't configured
         * on either the component or this resizer.
         *
         * @private
         */
        defaultMinSize: 50,

        /**
         * @property {RegExp} edgeDelimiterRe
         * A regex for splitting the edges by a separator when used as a string.
         *
         * @private
         */
        edgeDelimiterRe: /(?:\s*,\s*)|\s+/,

        /**
         * @property {Array} emptyConstrain
         * An empty array used to for {@link #minSize} and {@link #maxSize} when
         * no value is provided.
         *
         * @private
         */
        emptyConstrain: [null, null],

        /**
         * @property {Object} edgeInfoMap
         * Meta information about each edge.
         *
         * @private
         */
        edgeInfoMap: {
            north: {
                vert: true,
                constrainProp: {
                    vert: 'bottom'
                },
                adjustHeightOffset: -1,
                splitPosSetter: 'setY',
                oppSplitPosSetter: 'setX',
                sizeProp: 'height',
                startEdge: 'top',
                touchAction: { panY: false }
            },
            northeast: {
                horz: true,
                vert: true,
                corner: true,
                constrainProp: {
                    horz: 'left',
                    vert: 'bottom'
                },
                adjustHeightOffset: -1,
                adjustWidthOffset: 1,
                touchAction: { panX: false, panY: false }
            },
            east: {
                horz: true,
                constrainProp: {
                    horz: 'left'
                },
                adjustWidthOffset: 1,
                splitPosSetter: 'setX',
                oppSplitPosSetter: 'setY',
                sizeProp: 'width',
                startEdge: 'right',
                touchAction: { panX: false }
            },
            southeast: {
                horz: true,
                vert: true,
                corner: true,
                constrainProp: {
                    horz: 'left',
                    vert: 'top'
                },
                adjustHeightOffset: 1,
                adjustWidthOffset: 1,
                touchAction: { panX: false, panY: false }
            },
            south: {
                vert: true,
                constrainProp: {
                    vert: 'top'
                },
                adjustHeightOffset: 1,
                splitPosSetter: 'setY',
                oppSplitPosSetter: 'setX',
                sizeProp: 'height',
                startEdge: 'bottom',
                touchAction: { panY: false }
            },
            southwest: {
                horz: true,
                vert: true,
                corner: true,
                constrainProp: {
                    horz: 'right',
                    vert: 'top'
                },
                adjustHeightOffset: 1,
                adjustWidthOffset: -1,
                touchAction: { panX: false, panY: false }
            },
            west: {
                horz: true,
                constrainProp: {
                    horz: 'right'
                },
                adjustWidthOffset: -1,
                splitPosSetter: 'setX',
                oppSplitPosSetter: 'setY',
                sizeProp: 'width',
                startEdge: 'left',
                touchAction: { panX: false }
            },
            northwest: {
                horz: true,
                vert: true,
                corner: true,
                constrainProp: {
                    horz: 'right',
                    vert: 'bottom'
                },
                adjustHeightOffset: -1,
                adjustWidthOffset: -1,
                touchAction: { panX: false, panY: false }
            }
        },

        /**
         * @property {Object} positionMap
         * A map of short position names to long names.
         *
         * @private
         */
        positionMap: {
            n: 'north',
            north: 'north',
            ne: 'northeast',
            northeast: 'northeast',
            e: 'east',
            east: 'east',
            se: 'southeast',
            southeast: 'southeast',
            s: 'south',
            south: 'south',
            sw: 'southwest',
            southwest: 'southwest',
            w: 'west',
            west: 'west',
            nw: 'northwest',
            northwest: 'northwest'
        },

        sideInvertMap: {
            top: 'bottom',
            right: 'left',
            bottom: 'top',
            left: 'right'
        },

        splitEdges: {
            north: true,
            east: true,
            south: true,
            west: true
        },

        applyConstraintValue: function(v) {
            if (!Ext.isArray(v)) {
                v = [v, v];
            }
            return v;
        },

        calculateConstrain: function(targetVal, localVal, defaultValue) {
            var v = targetVal;

            if (v === null) {
                v = localVal;
            }

            if (v === null) {
                v = defaultValue;
            }

            return v;
        },

        createEdge: function(targetEl, pos) {
            var me = this,
                info = me.edgeInfoMap[pos],
                corner = info.corner,
                firstCorner = me.firstCorner,
                // Ensure corners are inserted later in the DOM so they appear "above" sides when hovering
                el = targetEl.createChild({
                    cls: me.baseCls + ' ' + Ext.baseCSSPrefix + pos,
                    'data-edge': pos
                }, corner ? null : firstCorner);

            el.setTouchAction(info.touchAction);
            if (corner) {
                el.dom.setAttribute('data-corner', 'true');
            }

            if (corner && !firstCorner) {
                me.firstCorner = el;
            }

            return el;
        },

        createProxy: function(edge, asFloat) {
            var me = this,
                proxyCls = me.proxyCls,
                orientationCls = edge.horz ? me.horizontalCls : me.verticalCls,
                modeCls = asFloat ? me.handleCls : me.splitterCls;

            return Ext.getBody().createChild({
                cls: proxyCls + ' ' + modeCls + ' ' + orientationCls
            });
        },

        cleanup: function() {
            var me = this,
                info = me.info,
                proxy = info && info.proxy;

            if (me.dragStarted) {
                if (proxy) {
                    proxy.destroy();
                }

                me.dragStarted = false;
                me.info = null;
            }
        },

        getBoxLayout: function() {
            var parent = this.getTarget().getParent(),
                layout = parent && parent.getLayout();

            return layout && layout.isBox ? layout : null;
        },

        getEdge: function(position) {
            position = this.positionMap[position];
            return this.edgeMap[position] || null;
        },

        getProxy: function() {
            var info = this.info;
            return info && info.proxy;
        },

        handleDrag: function(e) {
            if (!this.dragStarted) {
                return;
            }

            var info = this.info,
                target = info.target,
                edge = info.edge,
                asFloat = info.asFloat,
                box = info.startBox,
                horz = edge.horz,
                vert = edge.vert,
                offsetWidth = 0, 
                offsetHeight = 0,
                adjustWidthOffset = edge.adjustWidthOffset,
                adjustHeightOffset = edge.adjustHeightOffset,
                modifiesX = asFloat && edge.adjustWidthOffset < 0,
                modifiesY = asFloat && edge.adjustHeightOffset < 0,
                minHeight = info.minHeight,
                minWidth = info.minWidth,
                maxHeight = info.maxHeight,
                maxWidth = info.maxWidth,
                snappedWidth, snappedHeight, w, h, ratio,
                dragRatio, oppX, oppY, newBox;

            if (adjustWidthOffset) {
                offsetWidth = adjustWidthOffset * e.deltaX;
            }

            if (adjustHeightOffset) {
                offsetHeight = adjustHeightOffset * e.deltaY;
            }

            newBox = {
                width: box.width + offsetWidth,
                height: box.height + offsetHeight,
                x: box.x + (modifiesX ? -offsetWidth : 0),
                y: box.y + (modifiesY ? -offsetHeight : 0)
            };

            w = newBox.width;
            h = newBox.height;

            snappedWidth = horz ? this.snap(w, info.snapWidth, offsetWidth > 0) : w;
            snappedHeight = vert ? this.snap(h, info.snapHeight, offsetHeight > 0) : h;

            if (w !== snappedWidth || h !== snappedHeight) {
                if (modifiesX) {
                    newBox.x -= snappedWidth - w;
                }

                if (modifiesY) {
                    newBox.y -= snappedHeight - h;
                }
                newBox.width = w = snappedWidth;
                newBox.height = h = snappedHeight;
            }

            if (horz && (w < minWidth || w > maxWidth)) {
                newBox.width = w = Ext.Number.constrain(w, minWidth, maxWidth);

                if (modifiesX) {
                    newBox.x = box.x + (box.width - w);
                }
            }

            if (vert && (h < minHeight || h > maxHeight)) {
                newBox.height = h = Ext.Number.constrain(h, minHeight, maxHeight);

                if (modifiesY) {
                    newBox.y = box.y + (box.height - h);
                }
            }

            if (asFloat && (info.preserveRatio || e.shiftKey)) {
                ratio = info.ratio;

                h = Math.min(Math.max(minHeight, w / ratio), maxHeight);
                // Use newBox.height because we just overwrote h
                w = Math.min(Math.max(minWidth, newBox.height * ratio), maxWidth);

                if (horz && vert) {
                    // corner
                    oppX = box.x + (modifiesX ? box.width : 0);
                    oppY = box.y + (modifiesY ? box.height : 0);

                    dragRatio = Math.abs(oppX - e.pageX) / Math.abs(oppY - e.pageY);
                    if (dragRatio > ratio) {
                        newBox.height = h;
                    } else {
                        newBox.width = w;
                    }

                    if (modifiesX) {
                        newBox.x = box.x - (newBox.width - box.width);
                    }

                    if (modifiesY) {
                        newBox.y = box.y - (newBox.height - box.height);
                    }
                } else if (horz) {
                    // width only, adjust height to match
                    newBox.height = h;
                } else {
                    // height only, adjust width to match
                    newBox.width = w;
                }
            }

            if (target.hasListeners.resizedrag) {
                target.fireEvent('resizedrag', target, {
                    edge: info.edgeName,
                    event: e,
                    width: newBox.width,
                    height: newBox.height
                });
            }

            this.resize(newBox, e.type === 'dragend', e);
        },

        handleDragCancel: function(e) {
            var info = this.info,
                target = info.target;

            this.cleanup();

            if (target.hasListeners.resizedragcancel) {
                target.fireEvent('resizedragcancel', target, {
                    edge: info.edgeName,
                    event: e
                });
            }
        },

        handleDragEnd: function(e) {
            this.handleDrag(e);
            this.cleanup();
        },

        handleDragStart: function(e) {
            var me = this,
                emptyConstrain = me.emptyConstrain,
                target = me.getTarget(),
                hasListeners = target.hasListeners,
                dynamic = me.getDynamic(),
                edgeName = e.target.getAttribute('data-edge'),
                edge = me.edgeInfoMap[edgeName],
                horz = edge.horz,
                vert = edge.vert,
                region = target.element.getRegion(),
                snap = me.getSnap() || emptyConstrain,
                minSize = me.getMinSize() || emptyConstrain,
                maxSize = me.getMaxSize() || emptyConstrain,
                defaultMinSize = me.defaultMinSize,
                defaultMaxSize = me.defaultMaxSize,
                info, proxy, context, asFloat, layout, layoutVert, clearFlex;

            if (hasListeners.beforeresizedragstart) {
                context = {
                    edge: edgeName,
                    event: e
                };

                if (target.fireEvent('beforeresizedragstart', target, context) === false) {
                    return;
                }
            }

            asFloat = target.getFloated() || target.isPositioned();

            if (target.getFlex()) {
                layout = me.getBoxLayout();
                if (layout) {
                    layoutVert = layout.getVertical();
                    clearFlex = (horz && !layoutVert) || (vert && layoutVert);
                }
            }

            me.info = info = {
                target: target,
                edgeName: edgeName,
                dynamic: dynamic,
                startBox: region,
                snapHeight: snap[1],
                snapWidth: snap[0],
                clearFlex: clearFlex,
                minHeight: me.calculateConstrain(target.getMinHeight(), minSize[1], defaultMinSize),
                minWidth: me.calculateConstrain(target.getMinWidth(), minSize[0], defaultMinSize),
                maxHeight: me.calculateConstrain(target.getMaxHeight(), maxSize[1], defaultMaxSize),
                maxWidth: me.calculateConstrain(target.getMaxWidth(), maxSize[0], defaultMaxSize),
                edge: edge,
                asFloat: asFloat,
                preserveRatio: asFloat ? me.getPreserveRatio() : false,
                ratio: asFloat ? region.width / region.height : 0,
                start: region[edge.startEdge],
                floated: target.getFloated()
            };

            if (!dynamic) {
                info.proxy = proxy = me.createProxy(edge, asFloat);
                if (asFloat) {
                    proxy.setBox(region);
                } else {
                    proxy[edge.splitPosSetter](info.start);
                    proxy[edge.oppSplitPosSetter](horz ? region.top : region.left);
                    proxy.setSize(horz ? undefined : region.width, vert ? undefined : region.height);
                }
            }
            me.setupDragConstraints(info);

            me.dragStarted = true;

            if (hasListeners.resizedragstart) {
                target.fireEvent('resizedragstart', target, context || {
                    edge: edgeName,
                    event: e
                });
            }
            e.stopPropagation();
            // Prevent any further drag events from completing
            return false;
        },

        handleTouchStart: function(e) {
            // Used to prevent text selection
            e.preventDefault();
        },

        onTargetCollapse: function() {
            var me = this,
                map = me.edgeMap,
                key;

            me.disabled = true;
            me.dragListeners = Ext.destroy(me.dragListeners);

            for (key in map) {
                map[key].addCls(me.disabledCls);
            }
        },

        onTargetExpand: function() {
            var me = this,
                map = me.edgeMap,
                key;

            me.disabled = false;
            me.setupDragListeners();

            for (key in map) {
                map[key].removeCls(me.disabledCls);
            }
        },

        resize: function(newBox, atEnd, e) {
            var me = this,
                info = me.info,
                target = info.target,
                box = info.startBox,
                asFloat = info.asFloat,
                edge = info.edge,
                x = newBox.x,
                y = newBox.y,
                posChanged = asFloat && (box.x !== x || box.y !== y),
                horz = edge.horz,
                vert = edge.vert,
                floated = info.floated,
                onTarget = info.dynamic || atEnd,
                resizeTarget, isProxy, prop, diff, offset,
                targetParent, parentXY, positionEnd;

            if (onTarget) {
                resizeTarget = me.getTarget();
            } else {
                resizeTarget = info.proxy;
                isProxy = true;
            }

            if (!asFloat && isProxy) {
                prop = edge.sizeProp;
                offset = horz ? edge.adjustWidthOffset : edge.adjustHeightOffset;
                diff = (newBox[prop] - box[prop]) * offset;
                resizeTarget[edge.splitPosSetter](info.start + diff);
            } else {
                resizeTarget.setSize(horz ? newBox.width : undefined, vert ? newBox.height : undefined);
                if (!isProxy && info.clearFlex) {
                    resizeTarget.setFlex(null);
                }

                if (posChanged) {
                    positionEnd = !floated && onTarget;
                    if (positionEnd) {
                        targetParent = target.element.dom.parentNode;
                        parentXY = Ext.fly(targetParent).getXY();
                    }

                    if (horz) {
                        if (positionEnd) {
                            resizeTarget.setLeft(x - parentXY[0]);
                        } else {
                            resizeTarget.setX(x);
                        }
                    }
                    if (vert) {
                        if (positionEnd) {
                            resizeTarget.setTop(y - parentXY[1]);
                        } else {
                            resizeTarget.setY(y);
                        }
                    }
                }
            }

            if (atEnd) {
                if (target.hasListeners.resizedragend) {
                    target.fireEvent('resizedragend', target, {
                        edge: info.edgeName,
                        event: e,
                        width: newBox.width,
                        height: newBox.height
                    });
                }
            }
        },

        setupDragConstraints: function(info) {
            var me = this,
                dom = me.getTarget().element.dom,
                parent = dom.parentNode,
                clone = dom.cloneNode(false),
                fly = Ext.fly(clone),
                maxSize = me.defaultMaxSize,
                box, parentBox, edge, prop, invertMap;

            clone.style.position = 'absolute';

            fly.setMinHeight(info.minHeight);
            fly.setMinWidth(info.minWidth);
            fly.setMaxHeight(info.maxHeight);
            fly.setMaxWidth(info.maxWidth);

            // Make the fly really small, measure the width
            fly.setHeight(1);
            fly.setWidth(1);

            parent.appendChild(clone);
            info.minHeight = fly.getHeight();
            info.minWidth = fly.getWidth();

            // Make the fly really big
            fly.setHeight(maxSize);
            fly.setWidth(maxSize);

            info.maxHeight = fly.getHeight();
            info.maxWidth = fly.getWidth();

            if (me.getConstrainToParent()) {
                box = info.startBox;
                parentBox = Ext.fly(parent).getRegion();
                edge = info.edge;
                invertMap = me.sideInvertMap;

                if (edge.horz) {
                    prop = edge.constrainProp.horz;
                    info.maxWidth = Math.min(info.maxWidth, Math.abs(box[prop] - parentBox[invertMap[prop]]));
                }

                if (edge.vert) {
                    prop = edge.constrainProp.vert;
                    info.maxHeight = Math.min(info.maxHeight, Math.abs(box[prop] - parentBox[invertMap[prop]]));
                }
            }

            parent.removeChild(clone);
        },

        setupDragListeners: function() {
            var me = this,
                delegate = me.edgeDelegateSelector;

            me.dragListeners = me.getTarget().element.on({
                scope: me,
                destroyable: true,
                delegate: delegate,
                dragstart: {
                    // Higher priority so that we run before any draggable component handlers.
                    priority: 1000,
                    delegate: delegate,
                    fn: 'handleDragStart'
                },
                drag: 'handleDrag',
                dragend: 'handleDragEnd',
                dragcancel: 'handleDragCancel',
                touchstart: 'handleTouchStart'
            });
        },

        snap: function(value, increment, roundUp) {
            var m, m2;

            if (increment) {
                m = value % increment;
                if (m !== 0) {
                    m2 = m * 2;

                    value -= m;
                    if (roundUp && m2 >= increment) {
                        value += increment;
                    } else if (!roundUp && m2 > increment) {
                        value += increment;
                    } else if (m2 < -increment) {
                        value -= increment;
                    }
                }
            }
            return value;
        }
    }
});
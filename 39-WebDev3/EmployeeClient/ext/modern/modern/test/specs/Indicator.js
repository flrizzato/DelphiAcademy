topSuite('Ext.Indicator', ['Ext.app.ViewModel'], function () {
    var indicator;

    function createIndicator (config) {
        return indicator = new Ext.Indicator(config);
    }

    afterEach(function () {
        indicator = Ext.destroy(indicator);
    });

    describe('direction', function () {
        it('should have horizontal direction by default', function () {
            createIndicator();

            expect(indicator.getDirection()).toBe('horizontal');
            expect(indicator.element.hasCls('x-indicator-horizontal')).toBe(true);
        });

        it('should have vertical direction', function () {
            createIndicator({
                direction: 'vertical'
            });

            expect(indicator.getDirection()).toBe('vertical');
            expect(indicator.element.hasCls('x-indicator-vertical')).toBe(true);
        });

        it('should default to horizontal when invalid direction passed', function () {
            createIndicator();

            spyOn(Ext, 'raise');

            indicator.setDirection('foobar');

            expect(indicator.getDirection()).toBe('horizontal');
            expect(indicator.element.hasCls('x-indicator-horizontal')).toBe(true);

            expect(Ext.raise).toHaveBeenCalled();
        });
    });

    describe('indicators', function () {
        it('should add an indicator', function () {
            createIndicator();

            indicator
                .add()
                .add();

            expect(indicator.indicators.length).toBe(2);
        });

        it('should remove an indicator', function () {
            createIndicator();

            var indicators = indicator.indicators,
                first;

            indicator
                .add()
                .add();

            first = indicators[0];

            expect(indicators.length).toBe(2);

            indicator.remove();

            expect(indicators.length).toBe(1);
            expect(indicators[0]).toBe(first);
        });

        it('should remove all indicators', function () {
            createIndicator();

            var indicators = indicator.indicators;

            indicator
                .add()
                .add();

            expect(indicators.length).toBe(2);

            indicator.removeAll();

            expect(indicators.length).toBe(0);
        });
    });

    describe('active index', function () {
        it('should set dot active', function () {
            createIndicator();

            var indicators = indicator.indicators;

            indicator
                .add()
                .add();

            indicator.setActiveIndex(0);

            expect(indicators[0].hasCls('x-indicator-active')).toBe(true);
            expect(indicators[1].hasCls('x-indicator-active')).toBe(false);

            indicator.setActiveIndex(1);

            expect(indicators[0].hasCls('x-indicator-active')).toBe(false);
            expect(indicators[1].hasCls('x-indicator-active')).toBe(true);
        });

        it('should not set active to more than number of dots', function () {
            createIndicator();

            var indicators = indicator.indicators;

            indicator
                .add()
                .add();

            indicator.setActiveIndex(0);

            expect(indicators[0].hasCls('x-indicator-active')).toBe(true);

            function shouldThrow () {
                indicator.setActiveIndex(5);
            }

            expect(shouldThrow).toThrow();

            expect(indicators[0].hasCls('x-indicator-active')).toBe(true);
        });
    });

    describe('tap', function () {
        function createTapSpec (event, horizontal, touchValue) {
            it('should fire ' + event + ' event', function () {
                var touchProp = horizontal ? 'pageX' : 'pageY',
                    touch = {};

                touch[touchProp] = touchValue;

                indicator
                    .add()
                    .add();

                spyOn(indicator, 'fireEvent');

                indicator.onTap({
                    touch: touch
                });

                expect(indicator.fireEvent).toHaveBeenCalledWith(event, indicator);
            });
        }

        function createTapSuite (horizontal) {
            describe(horizontal ? 'horizontal' : 'vertical', function () {
                beforeEach(function () {
                    if (horizontal) {
                        createIndicator({
                            renderTo: Ext.getBody(),
                            height: 30,
                            width: 100
                        });
                    } else {
                        createIndicator({
                            renderTo: Ext.getBody(),
                            direction: 'vertical',
                            width: 30,
                            height: 100
                        });
                    }
                });

                createTapSpec('next',     horizontal, 75);
                createTapSpec('previous', horizontal, 40);
            });
        }

        //horizontal
        createTapSuite(true);
        //vertical
        createTapSuite();
    });

    describe('binding', function () {
        it('should bind to activeIndex', function () {
            createIndicator({
                renderTo: Ext.getBody(),
                bind: '{index}',
                viewModel: {}
            });

            indicator.add().add();

            var vm = indicator.getViewModel(),
                indicators = indicator.indicators;

            vm.set('index', 0);

            vm.notify();

            expect(indicator.getActiveIndex()).toBe(0);
            expect(indicators[0].hasCls('x-indicator-active')).toBe(true);
            expect(indicators[1].hasCls('x-indicator-active')).toBe(false);

            vm.set('index', 1);

            vm.notify();

            expect(indicator.getActiveIndex()).toBe(1);
            expect(indicators[0].hasCls('x-indicator-active')).toBe(false);
            expect(indicators[1].hasCls('x-indicator-active')).toBe(true);
        });
    });
});

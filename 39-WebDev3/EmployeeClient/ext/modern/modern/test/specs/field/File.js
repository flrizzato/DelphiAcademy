describe('Ext.field.File', function () {
    var field, fileButton;

    function createField (cfg) {
        field = new Ext.field.File(cfg);
        fileButton = field.getFileButton();
    }

    afterEach(function () {
        field = fileButton = Ext.destroy(field);
    });

    describe('fileButton', function () {
        it('should set buttonElement as an input', function () {
            createField({
                name: 'testFileField'
            });

            var input = fileButton.buttonElement.dom;

            expect(input.tagName).toBe('INPUT');

            // Name must go on the file input
            expect(input.name).toBe('testFileField');
        });

        it('should set type of input to file', function () {
            createField();

            var input = fileButton.buttonElement.dom;

            expect(input.type).toBe('file');
        });
    });
});

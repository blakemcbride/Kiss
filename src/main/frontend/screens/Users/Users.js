
/* global $$ */

'use strict';

(async function () {

    const WS = 'services/Users';

    const columnDefs = [
        {headerName: 'User Name', field: 'userName', width: 40 },
        {headerName: 'Password', field: 'userPassword', width: 40 },
        {headerName: 'Active', field: 'userActive', width: 20 }
    ];
    const grid = new AGGrid('users-grid', columnDefs, 'id');
    grid.show();

    // Initialize the Active dropdown
    $$('users-active').add('Y', 'Yes');
    $$('users-active').add('N', 'No');

    async function updateGrid() {
        grid.clear();
        $$('users-edit').disable();
        $$('users-delete').disable();
        const res = await Server.call(WS, 'getRecords');
        if (res._Success) {
            if (res.nodb) {
                Utils.showMessage('Error', 'The Users function cannot be used without a database.');
                $$('users-new').disable();
                return;
            }
            grid.addRecords(res.rows);
        }
    }

    updateGrid();

    $$('users-new').onclick(() => {
        $$('users-popup-title').setValue('Add User');
        $$('users-user-name').clear();
        $$('users-password').clear();
        $$('users-active').setValue('Y');
        Utils.popup_open('users-edit-popup', 'users-user-name');

        $$('users-ok').onclick(async () => {
            if ($$('users-user-name').isError('User Name'))
                return;
            if ($$('users-password').isError('Password'))
                return;
            const data = {
                userName: $$('users-user-name').getValue(),
                userPassword: $$('users-password').getValue(),
                userActive: $$('users-active').getValue()
            };
            const res = await Server.call(WS, 'addRecord', data);
            if (res._Success) {
                Utils.popup_close();
                updateGrid();
            }
        });

        $$('users-cancel').onclick(() => {
            Utils.popup_close();
        });
    });

    function edit() {
        const row = grid.getSelectedRow();
        $$('users-popup-title').setValue('Edit User');
        $$('users-user-name').setValue(row.userName);
        $$('users-password').setValue(row.userPassword);
        $$('users-active').setValue(row.userActive);
        Utils.popup_open('users-edit-popup', 'users-user-name');

        $$('users-ok').onclick(async () => {
            if ($$('users-user-name').isError('User Name'))
                return;
            if ($$('users-password').isError('Password'))
                return;
            const data = {
                id: row.id,
                userName: $$('users-user-name').getValue(),
                userPassword: $$('users-password').getValue(),
                userActive: $$('users-active').getValue()
            };
            const res = await Server.call(WS, 'updateRecord', data);
            if (res._Success) {
                Utils.popup_close();
                updateGrid();
            }
        });

        $$('users-cancel').onclick(() => {
            Utils.popup_close();
        });
    }

    $$('users-edit').onclick(edit);
    grid.setOnRowDoubleClicked(edit);

    grid.setOnSelectionChanged((rows) => {
        $$('users-edit').enable(rows);
        $$('users-delete').enable(rows);
    });

    $$('users-delete').onclick(() => {
        Utils.yesNo('Confirmation', 'Are you sure you want to delete the selected user?', async () => {
            const row = grid.getSelectedRow();
            const data = {
                id: row.id
            };
            const res = await Server.call(WS, 'deleteRecord', data);
            if (res._Success) {
                updateGrid();
            }
        });
    });

})();

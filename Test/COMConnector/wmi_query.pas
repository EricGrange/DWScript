var locator := CreateOleObject('WbemScripting.SWbemLocator');
var service := locator.ConnectServer('localhost', 'root\CIMV2', '', '');
var objectSet := service.ExecQuery('Select Name from Win32_Processor', 'WQL', $20);

for var item in objectSet do
   PrintLn(item.Properties_.Item('Name').Name);


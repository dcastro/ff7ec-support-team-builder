// Resources spreadsheet: https://docs.google.com/spreadsheets/d/1evoNzTA9veDRTvYJEMe-9F81QQ-CxUWN4mrd93kn2W4/edit?gid=0#gid=0
const spreadsheetId = "1evoNzTA9veDRTvYJEMe-9F81QQ-CxUWN4mrd93kn2W4"

// See: https://developers.google.com/sheets/api/reference/rest/v4/spreadsheets.values/get
export const _getSheet = range => async () => {
  return await gapi.client.request({'path': `https://sheets.googleapis.com/v4/spreadsheets/${spreadsheetId}/values/${range}`})
}

export function _loadGoogleApi() {
  return new Promise(function(resolve, reject) {
    gapi.load('client', async () => {
      await gapiLogin("AIzaSyARUnvuw1DvqJRISnPyOLEkqPvra4MF6fQ");
      resolve();
    });
  });
}


async function gapiLogin(key) {
  await gapi.client.init({
    'apiKey': key,
  });
}

import { api_call_get, format_date, show_error } from './helpers.js';
import { get_messages } from './messages.js'

export const populate_channel_details = (detailsObj) => {
    const channelDetailsDiv = document.getElementById("channel-details");
    channelDetailsDiv.innerHTML = `
    <h2>${detailsObj.name}</h2>
    <p>Created By: ${detailsObj.created_by}</p>
    <p>Time Created: ${detailsObj.time_created}</p>
    <p>Private: ${detailsObj.private}</p>
    <div id="descriptionDiv">Description: ${detailsObj.description}</div>
    `
}
export const get_channel_details = (body) => {
    api_call_get(`user/${body.creator}`, {}, true)
    .then((body1) => {
        const time_created= format_date(body.createdAt);
        populate_channel_details({
            name: body.name,
            private: body.private,
            time_created: time_created,
            created_by: body1.name,
            description: body.description,
        })

        get_messages()
        // fetch_all_messages(0, 25, [])
        // .then((allMessages) => {
        //     // console.log(allMessages)
        //     display_messages(allMessages, 0);
        // })
        // document.getElementById("main-messages").style.display = 'flex';
        // document.getElementById("message-container").style.display = 'flex';

        document.getElementById("leave-channel-button").style.display = 'block';
        document.getElementById("edit-channel-button").style.display = 'block';
        document.getElementById("page-join-page").style.display = "none";
    })
    .catch((msg) => {
        show_error(msg)
        // alert(msg);
        // edit the html error box to display current error message 'msg'
        // change html here to show in the error pop up
    });

}


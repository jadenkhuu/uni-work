Register.jsx
    We determined that a register screen should not have too much clutter, focusing the user attention on inputting their details.

    As such, the layout of the register screen is simple, having everything grouped into a div displayed in the middle of the screen, providing a simple green backdrop to provide a nice contrast in colour.

Login.jsx
    Implementing the login page was very similar to register, centering the main elements of the page in the middle of the screen and adding a background colour for contrast.

Dashboard.jsx
    Dashboard was a bit difficult to implement in the beginning, however, since it was provided in the specifications that the Logout button should always be present, we decided to make a navigation bar as the main component of the dashboard.

    There are a few buttons only at the top right of the nav bar which change from
        LOGIN / REGISTER
    to
        CREATE LISTING / YOURLISTINGS / LOGOUT
    once the user has logged in so they can differentiate if they are logged in or not.

    We used MUI's app bar as well as nested routes and react outlets to ensure the app bar component would follow the user around the website allowing the user to easily navigate to important sections of the app such as 'Your Listings' and 'All Listings' easily

    We also made the 'airbrb' text logo at the top right clickable, bringing the user back to the landing page. Thus, allowing the user to easily navigate back to the landing page if their current page has no back button.

YourListings.jsx
    YourListings mainly involved rendering the users created listings and displaying each listing on the page.

    The listing layout we ended up implementing was heavily inspired by AirBnb's listing layout. Each listing created would create a row until there was no more space in the row, going to the next row if another listing was created. Thus, creating a satisfying grid like appearance for the listing page

    The width of each individual listing is 340 pixels to accomodate for mobile users (we decided that 400px would be a decent mobile screen size). When the screen resolution is reduced or increased, the listings would utilise flexbox and rearrange themselves to fit the page dynamically. When reduced to a mobile device resolution, the listings can be seen as a single column of listings from a mobile perspective.

    Since this was also a page for the user to view their own listings and modify the listings, we simply added buttons to the bottom of each listing, EDIT, DELETE, and PUBLISH.

    Additional icons from MUI's material icons were also added so that the buttons have a bit more appeal.

    We also decided to make the thumbnail image clickable which would bring the user to the listing details page, showing the user the rest of the details to do with the listing. When hovered over, the thumbnail would enlarge, and the cursor would become a pointer, indicating that the image can be clicked on

    Compared to the listings in the AllListings page, each individual listing has a bit more information on it to help usabilty such as the address and price. The most important being the 'Published/Not Published' label, allowing the user to easily determine if their listings are live or not. To accompany this, the PUBLISH button also changed to UNPUBLISH when the listing is already live.

AllListings.jsx
    The implementation of AllListings is also very similar to YourListings. It mainly involved rendering each live listing on the page.

    Similarly to the YourListings page, the listings each have a width of 340px, allowing the listings to rearrange themselves when the screen size is reduced or increased, ending up as a single column of listings when viewed from a mobile device resolution.

    Also similarly to YourListings, the individual listings can be clicked on to view the full listing details, instead of only enlarging the image when hovered, the entire listing would enlarge a bit and would be highlighted a lighter shade of green, as well as changing the cursor to a pointer, indicating to the user that it can be clicked on.

    The listings shown on the AllListings page differ from the YourListings page a bit as it only displays the listing Title, price, and potential reviews. We decided to only show these details as any additional information may clutter up the screen when there are too many listings, as users can look at the listing in more detail when they click into it.

CreateListing.jsx
    CreateListing would bring the user to a separate page. The page is simply layed out with each input spanning across the screen. Each listing input box has a clear label about what the app is expecting to be inputted

    The notable inputs include:
        - the address, which split the address components into 4 parts, making it easier for the user to input their entire address

        - price, number of bathrooms, bedrooms and beds which only allow the user to enter numbers. The input allows the user to click the arrows at the end of the textbox and increment the number, users are also able to use up and down arrow keys to change the number

    Uploading a thumbnail allows the user to upload their desired thumbnail image for the listing. This input only allows one image at a time as there is only one thumbnail image. This input also shows a preview for the image so the user knows what image they are uploading

    Uploading additional images took some additional time to implement as we wanted to make the image upload as user friendly as possible. The user can upload up to 5 images, and each image would show a preview under the button similarly to the thumbnail upload. However, each preview for the additional images also have a red X on the top right which allows the user to remove the selected image if they change their mind.

    Inputting amenities included a single text box which has a + button next to it, allowing the user to input multiple amenities without the page having to show multiple text input boxes. The user can simply press the + button if they wish to add additional amenities, and remove them with the remove button if they change their mind also.

    Finally, a wide submit button allows for additional visibility, and a back button under it in case the user changes their mind about creating a new listing.

EditListing.jsx
    Edit listing is fundementally identical to the CreateListing page, as it simply allows the user to modify the details of their existing listing.

    We decided to make the layout identical because it would be familiar to the user, and it also simplified the implementation for us.

PublishListing.jsx
    PublishListing is also implemented simply, with a few buttons labelled with their function.

    We utilised MUI's date pickers to make picking the listings dates more appealing as well increase the usability as the user can clearly see on the calendar which date they are picking instead of simply entering a date

ViewListing.jsx
    With the ViewListing page, we wanted to make it very simple to navigate and read.

    We separated the main page into 3 sections, showing the images, main details, and additional ammenities.

    The images are displayed with the thumbnail being the biggest, and the additional images underneath. To make it easier to see the additional images, the user can hover over the image and it will enlarge to help visibility.

SearchListings.jsx
    SearchListing would use an on screen modal which allows the user to input any search filters they wish to apply to the listings.

    The button to access this modal is on the dashboard so the user can search for listings from whichever page they are on.

    Similarly to the other inputs we have in our app, the inputs clearly display what they are expecting to have inputted, making it simpler for the user to use.




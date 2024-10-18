from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from bs4 import BeautifulSoup
import pandas as pd
import time

# Set up Selenium WebDriver 
driver = webdriver.Chrome()
login_url = "https://www.marklines.com/en/members/login?fromSignUp=true"

# Navigate to the login page
driver.get(login_url)
time.sleep(1)

username = driver.find_element(By.ID, "profiles_login_login_id") 
password = driver.find_element(By.ID, "profiles_login_password")  

# Send credentials 
username.send_keys(" ") # Enter your username
password.send_keys(" ") # Enter your password
password.send_keys(Keys.RETURN)
time.sleep(1)  

years = [2024, 2023, 2022, 2021, 2020, 2019, 2018, 2017, 2016, 2015, 2014, 2013, 2012, 2011, 2010]
for year in years:
    data_url = f"https://www.marklines.com/en/vehicle_sales/month?nationCode=CHN&fromYear={year}"

    # Navigate to the page with the table after login
    driver.get(data_url)
    time.sleep(1)

    page_source = driver.page_source
    soup = BeautifulSoup(page_source, 'html.parser')
    table = soup.find('table', class_='table table-bordered aggregate_table')

    if table:
        headers = []
        header_row = table.find('tr', class_='aggregate_header')
        for th in header_row.find_all('th'):
            headers.append(th.text.strip())

        rows_data = []
        for row in table.find_all('tr'):
            row_data = []
            if 'aggregate_row' in row.get('class', []):
                row_data.append('')  
                row_data.append('')  
                row_data.append(row.find('th').text.strip())
                for td in row.find_all('td'):
                    row_data.append(td.text.strip())
                rows_data.append(row_data)

            else:

                # Fetch the rest <td> 
                for td in row.find_all('td'):
                    row_data.append(td.text.strip())

                # fetch the <th> of the row     
                level_0 = row.find('th', class_='aggregate_header level-0')
                level_1 = row.find('th', class_='aggregate_header level-1')
                row_header = row.find('th', class_='aggregate_row_header')

                row_data.insert(0, level_0.text.strip() if level_0 else '')  
                row_data.insert(1, level_1.text.strip() if level_1 else '')  
                row_data.insert(2, row_header.text.strip() if row_header else '')  

                rows_data.append(row_data)

        df = pd.DataFrame(rows_data)
        df.to_csv(f'/Users/chao/Downloads/vehicle_sales_{year}.csv', index=False)

    else:
        print("Table not found after logging in.")
    
# Close the browser
driver.quit()
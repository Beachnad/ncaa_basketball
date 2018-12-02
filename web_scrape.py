from bs4 import BeautifulSoup
import requests
import re
from datetime import date, timedelta

BASE_URL = "https://www.sports-reference.com"

date_range = lambda start, end: (start + timedelta(n) for n in range((end - start).days))


class WebScraper:
    def __init__(self):
        pass

    def get_soup(self, url):
        print(url)
        raw_html = requests.get(url)
        return BeautifulSoup(raw_html.text, 'html.parser')


class TeamPages(WebScraper):
    def iter_pages(self):
        url = f"{BASE_URL}/cfb/schools/"
        soup = self.get_soup(url)
        for row in soup.find('table', {'id': 'schools'}).find('tbody').find_all('tr'):
            if not bool({'thead', 'over_header'} & set(row.attrs.get('class', {}))):
                link = row.find('a', href=True)['href']
                page_soup = self.get_soup(f"{BASE_URL}{link}")
                page_soup.__setattr__('link', link)
                yield page_soup

    def get_data(self):
        data = {
            'id_name': [],
            'name': [],
            'city': [],
            'state': []
        }
        for pg in self.iter_pages():
            info_box = pg.find('div', {'id': 'info'})

            id_name = re.search('/cfb/schools/(.*)/', pg.link).group(1)
            name = info_box.find('h1').find('span').text
            location = re.search(
                'Location: (.*) ',
                info_box.find('strong', text='Location:').parent.text
            ).group(1).strip()
            city = re.search('(.*),.+', location).group(1)
            state = re.search('.*, (.+)', location).group(1)

            print(city, state, name)

            data['id_name'].append(id_name)
            data['name'].append(name)
            data['city'].append(city)
            data['state'].append(state)
        return data


class GamePages(WebScraper):
    def iter_pages(self, start: date, end: date):
        assert start < end
        for dt in date_range(start, end):
            if 5 < dt.month < 11: continue
            url = f'{BASE_URL}/cbb/boxscores/index.cgi?month={dt.month}&day={dt.day}&year={dt.year}'
            scores = self.get_soup(url)
            if scores.find('div', {'id': 'all_other_scores'}):
                for link in scores.find('div', {'id': 'all_other_scores'}).find_all('a', href=True, text='Final'):
                    soup = self.get_soup(f"{BASE_URL}{link['href']}")
                    soup.__setattr__('date', dt)
                    yield soup

    def get_data(self, start, end):
        DATA_KEYS = {
            'team_id','date', 'opp_id', 'loc', 'home_away_neutral', 'result',
            'fg','fga','fg2','fg2a','fg3','fg3a','ft', 'fta','orb',
            'drb','trb','ast','stl','blk','tov','pf','pts'}
        for pg in self.iter_pages(start, end):
            scorebox = pg.find('div', {'class': 'scorebox'})
            location = scorebox.find('div', {'class': 'scorebox_meta'}).find_all('div', recursive=False)[1].text

            away_sc, home_sc = scorebox.find_all('div', recursive=False)[:2]
            home_id = GamePages.get_team_id(home_sc)
            away_id = GamePages.get_team_id(away_sc)

            away_stats, home_stats = pg.find_all('table', {'class', 'stats_table'})
            home_stats = GamePages.get_team_data(home_stats)
            away_stats = GamePages.get_team_data(away_stats)

            for i, stats in enumerate((home_stats, away_stats)):

                team_id = (home_id, away_id)[i]
                opp_stats = (home_stats, away_stats)[0 if i == 1 else 1]
                opp_id = (home_id, away_id)[0 if i == 1 else 1]

                if not team_id:
                    continue
                try:
                    data = {'team_id': team_id, 'date': pg.date, 'opp_id': opp_id, 'loc': location,
                            'home_away_neutral': 'H' if i == 0 else 'A',
                            'result': 'W' if int(stats['pts']) > int(opp_stats['pts']) else 'L'}
                    for k, v in stats.items():
                        if k in DATA_KEYS:
                            data[k] = int(v)
                    yield data
                except ValueError:
                    continue


    @staticmethod
    def get_team_id(team_scorebox):
        try:
            team_id = re.search(
                '/cbb/schools/(.*)/\d{4}\.html',
                team_scorebox.find('strong').find('a', href=True)['href']
            ).group(1)
        except TypeError:
            team_id = None
        return team_id

    @staticmethod
    def get_team_data(team_stats_table):
        stats = {x.attrs['data-stat']: x.text for x in team_stats_table.find('tfoot').find_all('td')}
        return stats





if __name__ == "__main__":
    game_pages = GamePages()
    data = game_pages.get_data(date(2017, 12, 1), date(2017, 12, 5))

    print('...')
    i = 1
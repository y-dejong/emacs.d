import requests
from bs4 import BeautifulSoup
import os
import shutil
import time
import urllib
import argparse

def scrape_font_options():
    url = "https://patorjk.com/software/taag/#p=display&f=Graffiti&t=Type%20Something%20"
    try:
        response = requests.get(url, headers={'User-Agent': 'Firefox'})
        response.raise_for_status()
        soup = BeautifulSoup(response.text, 'html.parser')
        font_list = soup.find(id="fontList")
        if font_list:
            return [option['value'] for option in font_list.find_all('option')]
        return []
    except requests.exceptions.RequestException as e:
        print(f"Error fetching font options: {e}")
        return []

def download_fonts(fonts):
    opener = urllib.request.build_opener()
    opener.addheaders = [('User-Agent', 'Firefox')]
    urllib.request.install_opener(opener)
    for font in fonts:
        if os.path.exists(f"startup-banners/fonts/{font}") or not font.endswith(".flf"):
            continue

        url = f"https://patorjk.com/software/taag/fonts/{urllib.parse.quote(font)}"
        print(f"Downloading {font}...", end="")
        urllib.request.urlretrieve(url, f"startup-banners/fonts/{font}")
        print("Done")
        time.sleep(1)

def build_figlet():
    for program in ['tar', 'gcc', 'make']:
        if not shutil.which(program):
            print(f"Must have {program} installed to continue. Aborting")
            return

    # Download and unzip figlet
    figlet_name = "figlet-2.2.5"
    urllib.request.urlretrieve(f"http://ftp.figlet.org/pub/figlet/program/unix/{figlet_name}.tar.gz", f"{figlet_name}.tar.gz")
    shutil.unpack_archive(f"{figlet_name}.tar.gz")

    # Compile
    os.system(f"make -C {figlet_name} DEFAULTFONTDIR='fonts'")

    # Move figlet executable to fonts dir
    shutil.move(f"{figlet_name}/figlet", "startup-banners/")
    shutil.rmtree(figlet_name)
    os.remove(f"{figlet_name}.tar.gz")

def generate_banners(fonts):
    cwd = os.getcwd()

    os.chdir("startup-banners")
    for font in fonts:
        name = font.split(".")[0]
        os.system(f"./figlet -f \"{name}\" Emacs > \"{name}.txt\"")

def main():
    parser = argparse.ArgumentParser(description='Generate startup ASCII banners for dashboard')
    parser.add_argument('-d', '--emacs-dir', type=str, help='Output directory', default="~/.emacs.d/")
    parser.add_argument('-n', '--no-download', action='store_true', help='Do not download fonts')
    args = parser.parse_args()

    os.chdir(os.path.expanduser(args.emacs_dir))
    if not os.path.exists("startup-banners/fonts"):
        os.makedirs("startup-banners/fonts")
    if os.path.exists("startup-banners/figlet"):
        print("Found figlet")
    else:
        print("Figlet not found, downloading and building")
        build_figlet()

    if args.no_download:
        fonts = [os.path.splitext(filename)[0] for filename in os.listdir("startup-banners/fonts")]
    else:
        fonts = scrape_font_options()
        print("Got font options")
        print(fonts)
        download_fonts(fonts)

    generate_banners(fonts)

if __name__ == "__main__":
    main()

#!/usr/bin/env python3

# This script is expected to be ran from the terminal
# using the commands indicated in the terminal.txt file
# Python has to be installed on your computer

# This script downloads files from the Ocean Biology Distributed Active Archive Center (OB.DAAC)
# It supports authentication using .netrc files or app keys, handles compressed files, 
# and retries downloads in case of network issues.


# Required modules
import argparse  # For command-line argument parsing
import hashlib  # For file checksum verification
import os  # For file and directory operations
import re  # For regular expressions
import sys  # For system-level operations
import subprocess  # For running external commands
import logging  # For logging messages
import requests  # For handling HTTP requests
from requests.adapters import HTTPAdapter  # For configuring HTTP retries
from datetime import datetime  # For working with timestamps
import time  # For implementing retries with delays
import textwrap  # For text formatting in help messages
from urllib.parse import urlparse  # For parsing URLs
from pathlib import Path  # For modern path handling

# Default constants for download configuration
DEFAULT_CHUNK_SIZE = 131072  # Size of chunks for downloading files
BLOCKSIZE = 65536  # Block size for file checksum verification

# Global session object for maintaining HTTP connections
obpgSession = None

# Function to initialize or reuse an HTTP session
def getSession(verbose=0, ntries=5):
    """
    Initializes a global requests session for making HTTP requests.
    Reuses the session if already initialized.
    """
    global obpgSession

    if not obpgSession:
        # Enable debug logging if verbosity level is high
        if verbose > 1:
            print("Session started")
            logging.basicConfig(level=logging.DEBUG)

        # Create a new session and configure retry settings
        obpgSession = requests.Session()
        obpgSession.mount('https://', HTTPAdapter(max_retries=ntries))
    else:
        if verbose > 1:
            print("Reusing existing session")

    return obpgSession

# Function to check for authentication failures in HTTP responses
def isRequestAuthFailure(req):
    """
    Checks if an HTTP request failed due to authentication issues.
    """
    ctype = req.headers.get('Content-Type')
    if ctype and ctype.startswith('text/html'):
        if "<title>Earthdata Login</title>" in req.text:
            return True
    return False

# Main function for downloading a file
def httpdl(server, request, localpath='.', outputfilename=None, ntries=5,
           uncompress=False, timeout=30., verbose=0, force_download=False,
           chunk_size=DEFAULT_CHUNK_SIZE):
    """
    Downloads a file from the specified server and saves it to the local path.
    Supports retries, uncompression, and download progress reporting.
    """
    status = 0  # Status code for the download
    urlStr = 'https://' + server + request  # Full URL for the request
    retry_count = 0  # Retry counter
    max_retries = 3  # Maximum retry attempts
    
    global obpgSession
    localpath = Path(localpath)  # Ensure the local path is a Path object
    
    while retry_count < max_retries:
        try:
            # Initialize the session
            getSession(verbose=verbose, ntries=ntries)
            
            # Prepare HTTP headers
            headers = {}

            # Send the GET request with streaming enabled
            with obpgSession.get(urlStr, stream=True, timeout=timeout, headers=headers) as req:
                if req.status_code != 200:  # Check for HTTP errors
                    status = req.status_code
                    break
                elif isRequestAuthFailure(req):  # Check for authentication failures
                    status = 401
                    break
                else:
                    # Create the local path if it doesn't exist
                    if not Path.exists(localpath):
                        os.umask(0o02)
                        Path.mkdir(localpath, mode=0o2775, parents=True)

                    # Determine the output filename
                    if not outputfilename:
                        cd = req.headers.get('Content-Disposition')
                        if cd:
                            outputfilename = re.findall("filename=(.+)", cd)[0]
                        else:
                            outputfilename = urlStr.split('/')[-1]

                    ofile = localpath / outputfilename

                    # Prepare to download the file
                    total_length = req.headers.get('content-length')
                    if total_length is None:
                        if verbose:
                            print(f"Warning: Content-Length not provided for {outputfilename}")
                        total_length = 0
                    else:
                        total_length = int(total_length)
                        
                    if verbose > 0:
                        print(f"Downloading {outputfilename} ({total_length/1024/1024:.2f} MBs)")

                    temp_file = str(ofile) + '.temp'
                    length_downloaded = 0
                    
                    # Write the downloaded content to a temporary file
                    with open(temp_file, 'wb') as fd:
                        for chunk in req.iter_content(chunk_size=chunk_size):
                            if chunk:  # Process non-empty chunks
                                length_downloaded += len(chunk)
                                fd.write(chunk)
                                if verbose > 0 and total_length > 0:
                                    percent_done = int(50 * length_downloaded / total_length)
                                    sys.stdout.write("\r[%s%s]" % ('=' * percent_done, ' ' * (50-percent_done)))
                                    sys.stdout.flush()

                    # Rename the temporary file if the download was successful
                    if total_length == 0 or length_downloaded == total_length:
                        os.rename(temp_file, ofile)
                        # Uncompress the file if required
                        if uncompress:
                            if ofile.suffix in {'.Z', '.gz', '.bz2'}:
                                if verbose:
                                    print("\nUncompressing {}".format(ofile))
                                compressStatus = uncompressFile(ofile)
                                if compressStatus:
                                    status = compressStatus
                        status = 0
                        break  # Successful download
                    else:
                        # Remove incomplete file and raise an error
                        if os.path.exists(temp_file):
                            os.remove(temp_file)
                        raise requests.exceptions.ChunkedEncodingError(
                            f"Incomplete download: got {length_downloaded} of {total_length} bytes")

            if verbose:
                print("\n...Done")

        except (requests.exceptions.ChunkedEncodingError, 
                requests.exceptions.ConnectionError,
                requests.exceptions.ReadTimeout) as e:
            retry_count += 1
            if retry_count < max_retries:
                if verbose:
                    print(f"\nDownload failed, retrying ({retry_count}/{max_retries})...")
                time.sleep(5 * retry_count)  # Exponential backoff
                continue
            else:
                if verbose:
                    print(f"\nFailed after {max_retries} attempts")
                status = 1
                break

    return status

# Function to uncompress a file
def uncompressFile(compressed_file):
    """
    Uncompresses a file using the appropriate method based on its extension.
    """
    if compressed_file.suffix == '.Z':
        cmd = ['uncompress', compressed_file]
    elif compressed_file.suffix == '.gz':
        cmd = ['gunzip', compressed_file]
    elif compressed_file.suffix == '.bz2':
        cmd = ['bunzip2', compressed_file]
    else:
        return 0
    
    try:
        subprocess.run(cmd, check=True)
        return 0
    except subprocess.CalledProcessError:
        return 1

# Main section to process command-line arguments and download files
def main():
    """
    Main function to process command-line arguments and download files.
    """
    # Create an argument parser with comprehensive help text
    parser = argparse.ArgumentParser(
        description='Download files from MODIS/OB.DAAC servers.',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=textwrap.dedent('''
            Examples:
              python download.py --filelist urls.txt --odir data/
              python download.py --verbose --uncompress --filelist urls.txt
            ''')
    )
    
    # Required arguments
    parser.add_argument('--filelist', required=True, help='File containing URLs to download')
    
    # Optional arguments
    parser.add_argument('--odir', default='.', help='Output directory (default: current directory)')
    parser.add_argument('--verbose', action='store_true', help='Enable verbose output')
    parser.add_argument('--uncompress', action='store_true', help='Uncompress downloaded files')
    parser.add_argument('--timeout', type=float, default=30.0, help='Timeout for HTTP requests in seconds')
    parser.add_argument('--ntries', type=int, default=5, help='Number of retry attempts')
    
    # Parse the command-line arguments
    args = parser.parse_args()
    
    # Set verbosity level
    verbose = 1 if args.verbose else 0
    
    # Check if the file list exists
    if not os.path.exists(args.filelist):
        print(f"Error: File list '{args.filelist}' not found")
        return 1
    
    # Create the output directory if it doesn't exist
    os.makedirs(args.odir, exist_ok=True)
    
    # Read the URLs from the file list
    try:
        with open(args.filelist, 'r') as f:
            urls = [line.strip() for line in f if line.strip()]
    except Exception as e:
        print(f"Error reading file list: {e}")
        return 1
    
    if not urls:
        print("No URLs found in the file list")
        return 1
    
    if verbose:
        print(f"Found {len(urls)} URLs to download")
    
    # Download each URL
    fail_count = 0
    for i, url in enumerate(urls, 1):
        if verbose:
            print(f"\nProcessing URL {i}/{len(urls)}: {url}")
        
        # Parse the URL to get the server and path
        parsed_url = urlparse(url)
        server = parsed_url.netloc
        request = parsed_url.path
        
        if not server or not request:
            print(f"Invalid URL: {url}")
            fail_count += 1
            continue
        
        # Download the file
        status = httpdl(
            server=server,
            request=request,
            localpath=args.odir,
            uncompress=args.uncompress,
            timeout=args.timeout,
            verbose=verbose,
            ntries=args.ntries
        )
        
        if status != 0:
            print(f"Failed to download {url}, status code: {status}")
            fail_count += 1
    
    # Print a summary
    if verbose:
        print(f"\nDownload summary: {len(urls) - fail_count} successful, {fail_count} failed")
    
    return 0 if fail_count == 0 else 1

# Execute the main function when the script is run directly
if __name__ == "__main__":
    sys.exit(main())
# ============================================================
# EPA SHINY SHELL HELPERS - MINIMAL HEADER VERSION
# ============================================================
# Reusable EPA shell for Shiny apps.
#
# Includes:
# - EPA logo
# - EPA search bar
# - EPA top nav bar
# - page shell wrappers
#
# Footer is kept separately and can be reused as-is.
# ============================================================

epa_head <- function(page_title = "EPA App") {
  tags$head(
    HTML('
      <!-- Google Tag Manager -->
      <script>(function (w, d, s, l, i) {
      w[l] = w[l] || [];
      w[l].push({"gtm.start": new Date().getTime(), event: "gtm.js"});
      var f = d.getElementsByTagName(s)[0], j = d.createElement(s), dl = l != "dataLayer" ? "&l=" + l : "";
      j.async = true;
      j.src = "//www.googletagmanager.com/gtm.js?id=" + i + dl;
      f.parentNode.insertBefore(j, f);
      })(window, document, "script", "dataLayer", "GTM-L8ZB");</script>
      <!-- End Google Tag Manager -->
    '),
    
    tags$meta(charset = "utf-8"),
    tags$meta(`http-equiv` = "ImageToolbar", content = "false"),
    tags$meta(`httpequiv` = "cleartype", content = "on"),
    tags$meta(name = "HandheldFriendly", content = "true"),
    tags$meta(name = "MobileOptimized", content = "width"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    tags$link(
      rel = "icon",
      type = "image/x-icon",
      href = "https://www.epa.gov/themes/epa_theme/images/favicon.ico"
    ),
    
    HTML(paste0('<title>', page_title, '</title>')),
    
    tags$link(
      rel = "stylesheet",
      media = "all",
      href = "https://www.epa.gov/sites/all/libraries/standalone/css/core/style.css"
    ),
    tags$link(
      rel = "stylesheet",
      media = "all",
      href = "https://www.epa.gov/themes/epa_theme/css/styles.css?r6lsex"
    ),
    tags$link(
      rel = "stylesheet",
      media = "all",
      href = "https://cdnjs.cloudflare.com/ajax/libs/uswds/3.0.0-beta.3/css/uswds.min.css"
    ),
    tags$script(
      src = "https://cdnjs.cloudflare.com/ajax/libs/uswds/3.0.0-beta.3/js/uswds-init.min.js"
    ),
    
    # Minimal Shiny/bslib layout fixes
    tags$style(HTML("
      .container-fluid {
        padding-right: 0;
        padding-left: 0;
        margin-right: 0;
        margin-left: 0;
      }

      .tab-content {
        margin-right: 30px;
        margin-left: 30px;
      }

      .epa-app-wrapper {
        width: 100%;
      }

      .epa-app-main {
        padding-top: 0.5rem;
        padding-bottom: 1rem;
      }

      /* ==========================
         EPA HEADER
         ========================== */
      .login-header {
        background-color: #005ea2;
        padding: 1em 0;
        display: flex;
      }

      .login-header-inner {
        max-width: 60em;
        margin: 0 auto;
        padding: 0 1em;
        flex-grow: 1;
      }

      .site-name-and-slogan {
        display: flex;
        align-items: center;
        gap: 12px;
      }

      .site-name {
        margin: 0;
        line-height: 1;
      }

      .site-name a {
        display: inline-block;
      }

      .site-logo-svg-main {
        fill: #fff;
        display: block;
        height: 48px;
        width: auto;
      }

      .login-site-slogan {
        font-size: .8em;
        color: #fff;
        font-family: Arial, sans-serif;
        line-height: 1;
        margin-top: 4px;
      }

      .flex-growbox {
        flex-grow: 1;
      }

      header .region-header {
        display: flex;
        justify-content: right;
        align-items: center;
      }

      header .epa-search-form {
        display: flex;
        height: 28px;
        align-items: stretch;
      }

      header .form-text {
        height: 28px !important;
        width: 215px !important;
        border: 1px solid #fff;
        padding: 0 8px;
      }

      header .login-search-button {
        background-image: url('https://www.epa.gov/themes/epa_theme/images/search.svg'),
          linear-gradient(rgba(0, 0, 0, 0), rgba(0, 0, 0, 0));
        background-color: #1a4480;
        background-position: 50% 50%;
        background-repeat: no-repeat;
        background-size: 12px 12px;
        width: 44px;
        height: 28px;
        border: none;
      }

      /* ==========================
         EPA NAV BAR
         ========================== */
      nav.main-nav {
        background-color: #1a4480;
      }

      nav.main-nav .nav__inner {
        max-width: 60em;
        margin: 0 auto;
        padding: 0 1em;
      }

      nav.main-nav .menu {
        display: flex;
        flex-wrap: wrap;
        gap: 0;
        list-style: none;
        margin: 0;
        padding: 0;
      }

      nav.main-nav .menu-item {
        margin: 0;
      }

      nav.main-nav .menu-link {
        display: block;
        color: #fff;
        padding: 12px 16px;
        text-decoration: none;
        font-weight: 600;
      }

      nav.main-nav .menu-link:hover {
        background-color: #005ea2;
        color: #fff;
      }

      /* Hide mobile shell for minimal version */
      .mobile-nav {
        display: none;
      }

      @media screen and (max-width: 50em) {
        .login-site-slogan {
          display: none;
        }

        .login-header-inner {
          display: flex;
          flex-direction: column;
          align-items: flex-start;
          gap: 10px;
        }

        header .region-header {
          width: 100%;
          justify-content: flex-start;
        }

        header .form-text {
          width: 170px !important;
        }

        nav.main-nav .menu {
          flex-direction: column;
        }
      }
    "))
  )
}

epa_page_header <- function(contact_url = "https://www.epa.gov/home/forms/send-request-technical-support") {
  HTML(paste0('
    <div class="dialog-off-canvas-main-canvas" data-off-canvas-main-canvas>
      <main id="main" class="main" role="main" tabindex="-1">
      <div class="skip-links">
        <a href="#main-content" class="skip-link element-invisible element-focusable">Jump to main content</a>
      </div>

      <header class="clearfix login-header" role="banner">
        <div class="login-header-inner">
          <div class="site-name-and-slogan">
            <h1 class="site-name">
              <a href="https://www.epa.gov/" rel="home" title="Go to the home page">
                <svg class="site-logo-svg-main" aria-hidden="true" viewBox="0 0 454.1 154.7" xmlns="http://www.w3.org/2000/svg">
                  <path d="m112.8 61.2c-4.8 18.6-22.9 33.3-42.9 33.3-20.1 0-38-14.7-42.9-33.4h.2s9.8 10.3-.2 0c3.1 3.1 6.2 4.4 10.7 4.4s7.7-1.3 10.7-4.4c3.1 3.1 6.3 4.5 10.9 4.4 4.5 0 7.6-1.3 10.7-4.4 3.1 3.1 6.2 4.4 10.7 4.4s7.7-1.3 10.7-4.4c3.1 3.1 6.3 4.5 10.9 4.4 4.3 0 7.4-1.2 10.5-4.3z"></path>
                  <path d="m113.2 51.2c0-24-19.4-43.5-43.3-43.5-24 0-43.5 19.5-43.5 43.5h39.1c-4.8-1.8-8.1-6.3-8.1-11.6 0-7 5.7-12.5 12.5-12.5 7 0 12.7 5.5 12.7 12.5 0 5.2-3.1 9.6-7.6 11.6z"></path>
                  <path d="m72.6 147c.7-36.9 29.7-68.8 66.9-70 0 37.2-30 68-66.9 70z"></path>
                  <path d="m67.1 147c-.7-36.9-29.7-68.8-67.1-70 0 37.2 30.2 68 67.1 70z"></path>
                  <path d="m240 10.8h-87.9v133.1h87.9v-20.4h-60.3v-36h60.3v-21h-60.3v-35h60.3z"></path>
                  <path d="m272.8 66.5h27.1c9.1 0 15.2-8.6 15.1-17.7-.1-9-6.1-17.3-15.1-17.3h-25.3v112.4h-27.8v-133.1h62.3c20.2 0 35 17.8 35.2 38 .2 20.4-14.8 38.7-35.2 38.7h-36.3z"></path>
                  <path d="m315.9 143.9h29.7l12.9-35h54.2l-8.1-21.9h-38.4l18.9-50.7 39.2 107.6h29.7l-53.1-133.1h-33.7z"></path>
                </svg>
              </a>
            </h1>
            <div class="login-site-slogan">United States Environmental Protection Agency</div>
          </div>

          <div class="flex-growbox"></div>

          <div class="region-header">
            <div class="block-epa-core-gsa-epa-search" id="block-epa-core-gsa-epa-search">
              <form action="https://search.epa.gov/epasearch" class="epa-search-form" method="get">
                <label class="element-hidden" for="search-box">Search</label>
                <input class="form-text" id="search-box" name="querytext" placeholder="Search EPA.gov" value="">
                <button class="login-search-button" id="search-button" title="Search"></button>
                <input name="areaname" type="hidden" value="">
                <input name="areacontacts" type="hidden" value="">
                <input name="areasearchurl" type="hidden" value="">
                <input name="typeofsearch" type="hidden" value="epa">
                <input name="result_template" type="hidden" value="2col.ftl">
              </form>
            </div>
          </div>
        </div>
      </header>

      <nav class="nav main-nav clearfix" role="navigation">
        <div class="nav__inner">
          <h2 class="element-invisible">Main menu</h2>
          <ul class="menu" role="menu">
            <li class="menu-item" role="presentation">
              <a class="menu-link" href="https://www.epa.gov/environmental-topics" role="menuitem">Environmental Topics</a>
            </li>
            <li class="menu-item" role="presentation">
              <a class="menu-link" href="https://www.epa.gov/laws-regulations" role="menuitem">Laws &amp; Regulations</a>
            </li>
            <li class="menu-item" role="presentation">
              <a class="menu-link" href="https://www.epa.gov/report-violation" role="menuitem">Report a Violation</a>
            </li>
            <li class="menu-item" role="presentation">
              <a class="menu-link" href="https://www.epa.gov/aboutepa" role="menuitem">About EPA</a>
            </li>
          </ul>
        </div>
      </nav>

      <div class="l-page has-footer epa-app-wrapper">
        <div class="l-constrain">
          <div class="l-page__header">
            <div class="l-page__header-first">
              <div class="web-area-title"></div>
            </div>
            <div class="l-page__header-last">
              <a href="', contact_url, '" class="header-link">Contact Us</a>
            </div>
          </div>
          <article class="article epa-app-main">
  '))
}

epa_footer <- function() {
  HTML('
          </article>
        </div>
      </div>

      <footer class="main-footer clearfix" role="contentinfo">
        <div class="main-footer__inner">
          <div class="region-footer">
            <div class="block-pane-epa-global-footer" id="block-pane-epa-global-footer">
              <div class="row cols-3">
                <div class="col size-1of3">
                  <div class="col__title">Discover.</div>
                  <ul class="menu">
                    <li><a href="https://www.epa.gov/accessibility/epa-accessibility-statement">Accessibility Statement</a></li>
                    <li><a href="https://www.epa.gov/aboutepa/epas-administrator">EPA Administrator</a></li>
                    <li><a href="https://www.epa.gov/planandbudget">Budget &amp; Performance</a></li>
                    <li><a href="https://www.epa.gov/contracts">Contracting</a></li>
                    <li><a href="https://www.epa.gov/utilities/wwwepagov-snapshots">EPA www Web Snapshot</a></li>
                    <li><a href="https://www.epa.gov/grants">Grants</a></li>
                    <li><a href="https://www.epa.gov/ocr/whistleblower-protections-epa-and-how-they-relate-non-disclosure-agreements-signed-epa-employees">No FEAR Act Data</a></li>
                    <li><a href="https://www.epa.gov/web-policies-and-procedures/plain-writing">Plan Writing</a></li>
                    <li><a href="https://www.epa.gov/privacy">Privacy</a></li>
                    <li><a href="https://www.epa.gov/privacy/privacy-and-security-notice">Privacy and Security Notice</a></li>
                  </ul>
                </div>
                <div class="col size-1of3">
                  <div class="col__title">Connect.</div>
                  <ul class="menu">
                    <li><a href="https://www.data.gov/">Data.gov</a></li>
                    <li><a href="https://www.epaoig.gov/">Inspector General</a></li>
                    <li><a href="https://www.epa.gov/careers">Jobs</a></li>
                    <li><a href="https://www.epa.gov/newsroom">Newsroom</a></li>
                    <li><a href="https://www.regulations.gov/">Regulations.gov</a></li>
                    <li><a href="https://www.epa.gov/newsroom/email-subscriptions-epa-news-releases">Subscribe</a></li>
                    <li><a href="https://www.usa.gov/">USA.gov</a></li>
                  </ul>
                </div>
                <div class="col size-1of3">
                  <div class="col__title">EPA Sites.</div>
                  <ul class="menu">
                    <li><a href="https://www.epa.gov/aboutepa/epa-organizational-chart">Organization Chart</a></li>
                    <li><a href="https://www.epa.gov/aboutepa/epa-administrator">Administrator</a></li>
                    <li><a href="https://www.epa.gov/aboutepa/epa-regional-offices">Regional Offices</a></li>
                    <li><a href="https://www.epa.gov/aboutepa/epa-policies">Policies and Guidelines</a></li>
                    <li><a href="https://www.epa.gov/aboutepa/epa-web-policies-and-procedures">Web Policies &amp; Practices</a></li>
                  </ul>
                </div>
              </div>
            </div>
          </div>
        </div>
      </footer>
    </main>
  ')
}
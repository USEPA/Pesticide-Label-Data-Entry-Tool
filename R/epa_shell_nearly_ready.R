# ============================================================
# EPA SHINY SHELL HELPERS - AIRDATA / EPA PAGE STYLE
# ============================================================
# Header is left unchanged per request.
# Footer is centered and styled to match the newer EPA page styling:
# - dark gray background (#3d4551)
# - EPA seal on the left
# - Discover / Connect / Ask / Follow in one centered footer area
# - Follow is icon-only
# - no "Contact Us" footer line
# - banner toggle script retained
# ============================================================

epa_head <- function(page_title = "EPA App") {
  tags$head(
    HTML("
      <!-- Google Tag Manager -->
      <script>(function (w, d, s, l, i) {
      w[l] = w[l] || [];
      w[l].push({\"gtm.start\": new Date().getTime(), event: \"gtm.js\"});
      var f = d.getElementsByTagName(s)[0], j = d.createElement(s), dl = l != \"dataLayer\" ? \"&l=\" + l : \"\";
      j.async = true;
      j.src = \"//www.googletagmanager.com/gtm.js?id=\" + i + dl;
      f.parentNode.insertBefore(j, f);
      })(window, document, \"script\", \"dataLayer\", \"GTM-L8ZB\");</script>
      <!-- End Google Tag Manager -->
    "),
    tags$meta(charset = "utf-8"),
    tags$meta(`http-equiv` = "x-ua-compatible", content = "ie=edge"),
    tags$meta(name = "HandheldFriendly", content = "true"),
    tags$meta(name = "MobileOptimized", content = "width"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    tags$link(
      rel = "icon",
      type = "image/x-icon",
      href = "https://www.epa.gov/themes/epa_theme/images/favicon.ico"
    ),
    HTML(paste0("<title>", page_title, "</title>")),
    tags$link(
      rel = "preload",
      href = "https://www.epa.gov/themes/epa_theme/fonts/source-sans-pro/sourcesanspro-regular-webfont.woff2",
      as = "font",
      crossorigin = "anonymous"
    ),
    tags$link(
      rel = "preload",
      href = "https://www.epa.gov/themes/epa_theme/fonts/source-sans-pro/sourcesanspro-bold-webfont.woff2",
      as = "font",
      crossorigin = "anonymous"
    ),
    tags$link(
      rel = "preload",
      href = "https://www.epa.gov/themes/epa_theme/fonts/merriweather/Latin-Merriweather-Bold.woff2",
      as = "font",
      crossorigin = "anonymous"
    ),
    tags$link(
      rel = "stylesheet",
      media = "all",
      href = "https://www.epa.gov/core/themes/stable/css/system/components/ajax-progress.module.css?r6lsex"
    ),
    tags$link(
      rel = "stylesheet",
      media = "all",
      href = "https://www.epa.gov/core/themes/stable/css/system/components/autocomplete-loading.module.css?r6lsex"
    ),
    tags$link(
      rel = "stylesheet",
      media = "all",
      href = "https://www.epa.gov/core/themes/stable/css/system/components/js.module.css?r6lsex"
    ),
    tags$link(
      rel = "stylesheet",
      media = "all",
      href = "https://www.epa.gov/core/themes/stable/css/system/components/sticky-header.module.css?r6lsex"
    ),
    tags$link(
      rel = "stylesheet",
      media = "all",
      href = "https://www.epa.gov/themes/epa_theme/css/styles.css?r6lsex"
    ),
    tags$link(
      rel = "stylesheet",
      media = "all",
      href = "https://www.epa.gov/themes/epa_theme/css-lib/colorbox.min.css?r6lsex"
    ),
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "https://cdnjs.cloudflare.com/ajax/libs/uswds/3.0.0-beta.3/css/uswds.min.css"
    ),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/uswds/3.0.0-beta.3/js/uswds-init.min.js"),
    
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

      body,
      .usa-banner,
      .login-header,
      .footer,
      .login-site-slogan,
      .menu-link,
      .header-link,
      .usa-banner__header-text,
      .usa-banner__header-action {
        font-family: 'Source Sans Pro', Arial, sans-serif;
      }

      h1, h2, h3, h4, h5, h6 {
        font-family: 'Merriweather', Georgia, serif;
      }

      .epa-app-wrapper {
        width: 100%;
      }

      .epa-app-main {
        padding-top: 0.25rem;
        padding-bottom: 1rem;
      }

      .usa-banner {
        text-align: left;
      }

      .usa-banner__header {
        text-align: left;
      }

      .usa-banner__inner {
        width: 100%;
        max-width: 1200px;
        margin: 0 auto;
        padding: 0 1rem;
        display: flex;
        align-items: center;
        justify-content: flex-start;
        gap: 0.75rem;
        flex-wrap: nowrap;
      }

      .usa-banner__header-text,
      .usa-banner__header-action {
        margin: 0;
        text-align: left;
      }

      .usa-banner__button {
        margin-left: auto;
        flex: 0 0 auto;
      }

      .usa-banner__button-text {
        white-space: nowrap;
      }

      .usa-accordion__content {
        display: none;
      }

      .usa-accordion__content.is-visible,
      .usa-accordion__content[aria-hidden='false'] {
        display: block;
      }

      .usa-banner__content {
        overflow: hidden;
      }

      .login-header {
        background-color: #005ea2;
        padding: 1em 0;
        display: flex;
        justify-content: flex-start;
      }

      .login-header-inner {
        width: 100%;
        max-width: 1200px;
        margin: 0;
        padding: 0 1rem;
        display: flex;
        align-items: center;
        flex-wrap: nowrap;
        gap: 1rem;
      }

      .site-name-and-slogan {
        display: flex;
        align-items: center;
        gap: 14px;
        flex: 0 0 auto;
        min-width: 0;
      }

      .site-name {
        margin: 0;
        line-height: 1;
        flex: 0 0 auto;
      }

      .site-name a {
        display: inline-block;
      }

      .site-logo-svg-main {
        fill: #fff;
        display: block;
        height: 44px;
        width: auto;
        flex: 0 0 auto;
      }

      .login-site-slogan {
        font-size: 0.82em;
        color: #fff;
        line-height: 1.15;
        margin: 0;
        white-space: nowrap;
        flex: 0 0 auto;
        padding-top: 2px;
      }

      .flex-growbox {
        flex-grow: 1;
      }

      header .region-header {
        display: flex;
        justify-content: flex-end;
        align-items: center;
      }

      header .block-epa-core-gsa-epa-search {
        margin-left: auto;
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
        font-size: 0.95rem;
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

      nav.main-nav {
        background-color: #1a4480;
        width: 100%;
      }

      nav.main-nav .nav__inner {
        max-width: 1200px;
        margin: 0;
        padding: 0 1rem;
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

      .mobile-nav {
        display: none;
      }

      /* =========================================================
         EPA PAGE FOOTER STYLE
         ========================================================= */

      .epa-contact-updated {
        display: none;
      }

      .footer {
        background: #3d4551;
        color: #ffffff;
        padding: 18px 0 24px 0;
        font-family: 'Source Sans Pro', Arial, sans-serif;
      }

      .footer .l-constrain {
        max-width: 1200px;
        margin: 0 auto;
        padding: 0 1rem;
      }

      .footer__pre-footer {
        border-bottom: 1px solid rgba(255, 255, 255, 0.18);
        padding-bottom: 10px;
        margin-bottom: 16px;
      }

      .list--footer-language-bar {
        list-style: none;
        margin: 0;
        padding: 0;
        display: flex;
        flex-wrap: wrap;
        justify-content: center;
        gap: 0.75rem 1rem;
      }

      .list--footer-language-bar li {
        margin: 0;
        padding: 0;
      }

      .list--footer-language-bar a {
        color: #ffffff;
        text-decoration: underline;
        font-size: 0.95rem;
        font-family: 'Source Sans Pro', Arial, sans-serif;
      }

      .list--footer-language-bar a:hover {
        color: #dfe1e2;
      }

      .footer__content {
        display: flex;
        align-items: flex-start;
        gap: 2rem;
        justify-content: center;
        flex-wrap: nowrap;
      }

      .footer__epa-seal {
        flex: 0 0 100px;
        width: 100px;
        height: 100px;
        display: block;
        margin-top: 2px;
      }

      .footer__column {
        flex: 1 1 0;
        min-width: 0;
      }

      .footer__column h1,
      .footer__column h2 {
        margin: 0 0 10px 0;
        font-size: 0.92rem;
        font-weight: 700;
        color: #ffffff;
        line-height: 1.2;
        font-family: 'Source Sans Pro', Arial, sans-serif;
      }

      .menu--footer,
      .menu--social {
        list-style: none;
        margin: 0;
        padding: 0;
      }

      .menu--footer li,
      .menu--social li {
        margin: 0 0 6px 0;
        padding: 0;
        line-height: 1.35;
      }

      .menu--footer a,
      .menu--social a {
        color: #ffffff;
        text-decoration: underline;
        font-size: 0.95rem;
        font-family: 'Source Sans Pro', Arial, sans-serif;
      }

      .menu--footer a:hover,
      .menu--social a:hover {
        color: #dfe1e2;
      }

      .menu--social {
        display: flex;
        gap: 0.65rem;
        flex-wrap: wrap;
        align-items: center;
        margin-top: 4px;
      }

      .menu--social .menu__item {
        margin: 0;
      }

      .menu--social .menu__link {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        width: 28px;
        height: 28px;
        text-decoration: none;
        color: #ffffff;
      }

      .menu--social .menu__link:hover {
        color: #dfe1e2;
      }

      .menu--social .icon--social {
        width: 22px;
        height: 22px;
      }

      /* Center the footer content as a single group */
      .footer__content {
        justify-content: center;
        align-items: flex-start;
      }

      .footer__content > a {
        flex: 0 0 100px;
        display: flex;
        justify-content: center;
        align-items: flex-start;
      }

      .footer__content > .footer__column {
        max-width: 220px;
      }

      @media (max-width: 767px) {
        .footer__content {
          flex-direction: column;
          align-items: stretch;
        }

        .footer__content > a {
          margin: 0 auto 8px auto;
        }
      }

      @media screen and (max-width:50em) {
        header .login-site-slogan {
          display: none;
        }

        .login-header-inner {
          display: flex;
          flex-direction: column;
          align-items: center;
        }

        .mobile-links {
          margin-top: 0;
        }

        .menu-button {
          display: none;
        }
      }
    "))
  )
}

epa_banner <- function() {
  HTML("
    <div class='dialog-off-canvas-main-canvas' data-off-canvas-main-canvas>
      <section class='usa-banner' aria-label='Official government website'>
        <div class='usa-accordion'>
          <header class='usa-banner__header'>
            <div class='usa-banner__inner'>
              <div class='grid-col-auto'>
                <img class='usa-banner__header-flag'
                     src='https://www.epa.gov/themes/epa_theme/images/us_flag_small.png'
                     alt='U.S. flag' />
              </div>
              <div class='grid-col-fill tablet:grid-col-auto'>
                <p class='usa-banner__header-text'>An official website of the United States government</p>
                <p class='usa-banner__header-action' aria-hidden='true'>Here’s how you know</p>
              </div>
              <button class='usa-accordion__button usa-banner__button'
                      aria-expanded='false'
                      aria-controls='gov-banner'
                      type='button'>
                <span class='usa-banner__button-text'>Here’s how you know</span>
              </button>
            </div>
          </header>

          <div class='usa-banner__content usa-accordion__content' id='gov-banner' aria-hidden='true'>
            <div class='grid-row grid-gap-lg'>
              <div class='usa-banner__guidance tablet:grid-col-6'>
                <img class='usa-banner__icon usa-media-block__img'
                     src='https://www.epa.gov/themes/epa_theme/images/icon-dot-gov.svg'
                     alt='Dot gov'>
                <div class='usa-media-block__body'>
                  <p>
                    <strong>Official websites use .gov</strong>
                    <br> A <strong>.gov</strong> website belongs to an official government organization in the United States.
                  </p>
                </div>
              </div>

              <div class='usa-banner__guidance tablet:grid-col-6'>
                <img class='usa-banner__icon usa-media-block__img'
                     src='https://www.epa.gov/themes/epa_theme/images/icon-https.svg'
                     alt='HTTPS'>
                <div class='usa-media-block__body'>
                  <p>
                    <strong>Secure .gov websites use HTTPS</strong>
                    <br> A <strong>lock</strong> indicates a secure connection.
                  </p>
                </div>
              </div>
            </div>
          </div>
        </div>
      </section>
    </div>
  ")
}

epa_page_header <- function() {
  HTML("
    <div class='dialog-off-canvas-main-canvas' data-off-canvas-main-canvas>
      <div class='skip-links'>
        <a href='#main-content' class='skip-link element-invisible element-focusable'>Jump to main content</a>
      </div>

      <header class='clearfix login-header' role='banner'>
        <div class='login-header-inner'>
          <div class='site-name-and-slogan'>
            <h1 class='site-name'>
              <a href='https://www.epa.gov/' rel='home' title='Go to the home page'>
                <svg class='site-logo-svg-main' aria-hidden='true' viewBox='0 0 454.1 154.7' xmlns='http://www.w3.org/2000/svg'>
                  <path d='m112.8 61.2c-4.8 18.6-22.9 33.3-42.9 33.3-20.1 0-38-14.7-42.9-33.4h.2s9.8 10.3-.2 0c3.1 3.1 6.2 4.4 10.7 4.4s7.7-1.3 10.7-4.4c3.1 3.1 6.3 4.5 10.9 4.4 4.5 0 7.6-1.3 10.7-4.4 3.1 3.1 6.2 4.4 10.7 4.4s7.7-1.3 10.7-4.4c3.1 3.1 6.3 4.5 10.9 4.4 4.3 0 7.4-1.2 10.5-4.3z'></path>
                  <path d='m113.2 51.2c0-24-19.4-43.5-43.3-43.5-24 0-43.5 19.5-43.5 43.5h39.1c-4.8-1.8-8.1-6.3-8.1-11.6 0-7 5.7-12.5 12.5-12.5 7 0 12.7 5.5 12.7 12.5 0 5.2-3.1 9.6-7.6 11.6z'></path>
                  <path d='m72.6 147c.7-36.9 29.7-68.8 66.9-70 0 37.2-30 68-66.9 70z'></path>
                  <path d='m67.1 147c-.7-36.9-29.7-68.8-67.1-70 0 37.2 30.2 68 67.1 70z'></path>
                  <path d='m240 10.8h-87.9v133.1h87.9v-20.4h-60.3v-36h60.3v-21h-60.3v-35h60.3z'></path>
                  <path d='m272.8 66.5h27.1c9.1 0 15.2-8.6 15.1-17.7-.1-9-6.1-17.3-15.1-17.3h-25.3v112.4h-27.8v-133.1h62.3c20.2 0 35 17.8 35.2 38 .2 20.4-14.8 38.7-35.2 38.7h-36.3z'></path>
                  <path d='m315.9 143.9h29.7l12.9-35h54.2l-8.1-21.9h-38.4l18.9-50.7 39.2 107.6h29.7l-53.1-133.1h-33.7z'></path>
                </svg>
              </a>
            </h1>
            <div class='login-site-slogan'>United States Environmental Protection Agency</div>
          </div>

          <div class='flex-growbox'></div>

          <div class='region-header'>
            <div class='block-epa-core-gsa-epa-search' id='block-epa-core-gsa-epa-search'>
              <form action='https://search.epa.gov/epasearch' class='epa-search-form' method='get'>
                <label class='element-hidden' for='search-box'>Search</label>
                <input class='form-text' id='search-box' name='querytext' placeholder='Search EPA.gov' value=''>
                <button class='login-search-button' id='search-button' title='Search'></button>
                <input name='areaname' type='hidden' value=''>
                <input name='areacontacts' type='hidden' value=''>
                <input name='areasearchurl' type='hidden' value=''>
                <input name='typeofsearch' type='hidden' value='epa'>
                <input name='result_template' type='hidden' value='2col.ftl'>
              </form>
            </div>
          </div>
        </div>
      </header>

      <nav class='nav main-nav clearfix' role='navigation'>
        <div class='nav__inner'>
          <h2 class='element-invisible'>Main menu</h2>
          <ul class='menu' role='menu'>
            <li class='menu-item' role='presentation'>
              <a class='menu-link' href='https://www.epa.gov/environmental-topics' role='menuitem'>Environmental Topics</a>
            </li>
            <li class='menu-item' role='presentation'>
              <a class='menu-link' href='https://www.epa.gov/laws-regulations' role='menuitem'>Laws &amp; Regulations</a>
            </li>
            <li class='menu-item' role='presentation'>
              <a class='menu-link' href='https://www.epa.gov/report-violation' role='menuitem'>Report a Violation</a>
            </li>
            <li class='menu-item' role='presentation'>
              <a class='menu-link' href='https://www.epa.gov/aboutepa' role='menuitem'>About EPA</a>
            </li>
          </ul>
        </div>
      </nav>

      <main id='main' class='main' role='main' tabindex='-1'>
      <div class='l-page has-footer epa-app-wrapper'>
        <div class='l-constrain'>
          <div class='l-page__header'>
            <div class='l-page__header-first'>
              <div class='web-area-title'></div>
            </div>
          </div>
          <article class='article epa-app-main'>
  ")
}

epa_footer <- function() {
  tagList(
    HTML("
          </article>
        </div>
      </div>
    </main>

    <footer class='footer' role='contentinfo'>
      <div class='l-constrain'>
        <div class='footer__pre-footer'>
          <ul class='list list--footer-language-bar'>
            <li><a href='https://www.epa.gov/lep/assistance'>Assistance</a></li>
            <li><a href='https://espanol.epa.gov/'>Spanish</a></li>
            <li><a href='https://www.epa.gov/lep/arabic'>Arabic</a></li>
            <li><a href='https://www.epa.gov/lep/simplified-chinese'>Chinese (simplified)</a></li>
            <li><a href='https://www.epa.gov/lep/traditional-chinese'>Chinese (traditional)</a></li>
            <li><a href='https://www.epa.gov/lep/french'>French</a></li>
            <li><a href='https://www.epa.gov/lep/sante-ak-anviwonman-enfomasyon-kreyol-ayisyen'>Haitian Creole</a></li>
            <li><a href='https://www.epa.gov/lep/korean'>Korean</a></li>
            <li><a href='https://www.epa.gov/lep/saude-e-informacoes-ambientais-em-portugues'>Portuguese</a></li>
            <li><a href='https://www.epa.gov/lep/russian'>Russian</a></li>
            <li><a href='https://www.epa.gov/lep/tagalog'>Tagalog</a></li>
            <li><a href='https://www.epa.gov/lep/vietnamese'>Vietnamese</a></li>
          </ul>
        </div>

        <div class='footer__content'>
          <a href='https://www.epa.gov/' aria-label='United States Environmental Protection Agency home'>
            <img class='footer__epa-seal' src='https://www.epa.gov/themes/epa_theme/images/epa-seal.svg' alt='United States Environmental Protection Agency'>
          </a>

          <div class='footer__column'>
            <h1>Discover.</h1>
            <ul class='menu menu--footer'>
              <li><a href='https://www.epa.gov/accessibility/epa-accessibility-statement'>Accessibility Statement</a></li>
              <li><a href='https://www.epa.gov/planandbudget'>Budget &amp; Performance</a></li>
              <li><a href='https://www.epa.gov/contracts'>Contracting</a></li>
              <li><a href='https://www.epa.gov/utilities/wwwepagov-snapshots'>EPA www Web Snapshot</a></li>
              <li><a href='https://www.epa.gov/grants'>Grants</a></li>
              <li><a href='https://www.epa.gov/ocr/no-fear-act-data'>No FEAR Act Data</a></li>
              <li><a href='https://www.epa.gov/web-policies-and-procedures/plain-writing'>Plain Writing</a></li>
              <li><a href='https://www.epa.gov/privacy/privacy-and-security-notice'>Privacy and Security Notice</a></li>
            </ul>
          </div>

          <div class='footer__column'>
            <h1>Connect.</h1>
            <ul class='menu menu--footer'>
              <li><a href='https://www.data.gov/'>Data.gov</a></li>
              <li><a href='https://www.epaoig.gov/'>Inspector General</a></li>
              <li><a href='https://www.epa.gov/careers'>Jobs</a></li>
              <li><a href='https://www.epa.gov/newsroom'>Newsroom</a></li>
              <li><a href='https://www.regulations.gov/'>Regulations.gov</a></li>
              <li><a href='https://www.epa.gov/newsroom/email-subscriptions-epa-news-releases'>Subscribe</a></li>
              <li><a href='https://www.usa.gov/'>USA.gov</a></li>
              <li><a href='https://www.whitehouse.gov/'>White House</a></li>
            </ul>
          </div>

          <div class='footer__column'>
            <h1>Ask.</h1>
            <ul class='menu menu--footer'>
              <li><a href='https://www.epa.gov/home/forms/contact-epa'>Contact EPA</a></li>
              <li><a href='https://www.epa.gov/web-policies-and-procedures/epa-disclaimers'>EPA Disclaimers</a></li>
              <li><a href='https://www.epa.gov/aboutepa/epa-hotlines'>Hotlines</a></li>
              <li><a href='https://www.epa.gov/foia'>FOIA Requests</a></li>
              <li><a href='https://www.epa.gov/home/frequent-questions-specific-epa-programstopics'>Frequent Questions</a></li>
            </ul>

            <h1>Follow.</h1>
            <ul class='menu menu--social' aria-label='EPA social media links'>
              <li class='menu__item'>
                <a class='menu__link' aria-label='EPA Facebook' href='https://www.facebook.com/EPA'>
                  <svg class='icon icon--social' aria-hidden='true'><use href='https://www.epa.gov/themes/epa_theme/images/sprite.artifact.svg#facebook-square'></use></svg>
                </a>
              </li>
              <li class='menu__item'>
                <a class='menu__link' aria-label='EPA Twitter / X' href='https://x.com/epa'>
                  <svg class='icon icon--social' aria-hidden='true'><use href='https://www.epa.gov/themes/epa_theme/images/sprite.artifact.svg#twitter-square'></use></svg>
                </a>
              </li>
              <li class='menu__item'>
                <a class='menu__link' aria-label='EPA YouTube' href='https://www.youtube.com/user/USEPAgov'>
                  <svg class='icon icon--social' aria-hidden='true'><use href='https://www.epa.gov/themes/epa_theme/images/sprite.artifact.svg#youtube-square'></use></svg>
                </a>
              </li>
              <li class='menu__item'>
                <a class='menu__link' aria-label='EPA Instagram' href='https://www.instagram.com/epagov'>
                  <svg class='icon icon--social' aria-hidden='true'><use href='https://www.epa.gov/themes/epa_theme/images/sprite.artifact.svg#instagram-square'></use></svg>
                </a>
              </li>
            </ul>
          </div>
        </div>
      </div>
    </footer>
  "),
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
        var btn = document.querySelector('.usa-banner__button');
        var content = document.getElementById('gov-banner');
        if (!btn || !content) return;

        content.style.display = 'none';
        content.setAttribute('aria-hidden', 'true');
        btn.setAttribute('aria-expanded', 'false');

        btn.addEventListener('click', function(e) {
          e.preventDefault();
          var expanded = btn.getAttribute('aria-expanded') === 'true';
          btn.setAttribute('aria-expanded', expanded ? 'false' : 'true');
          content.style.display = expanded ? 'none' : 'block';
          content.setAttribute('aria-hidden', expanded ? 'true' : 'false');
        });
      });
    "))
  )
}
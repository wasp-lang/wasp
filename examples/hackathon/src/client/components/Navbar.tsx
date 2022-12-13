import React from 'react';
import waspLogo from '../waspLogo.png';
import { Star } from 'react-feather';

export const DiscordIcon = () => (
  <svg width='24' height='24' fill='currentColor' viewBox='0 5 30.67 23.25'>
    <title>Discord</title>
    <path d='M26.0015 6.9529C24.0021 6.03845 21.8787 5.37198 19.6623 5C19.3833 5.48048 19.0733 6.13144 18.8563 6.64292C16.4989 6.30193 14.1585 6.30193 11.8336 6.64292C11.6166 6.13144 11.2911 5.48048 11.0276 5C8.79575 5.37198 6.67235 6.03845 4.6869 6.9529C0.672601 12.8736 -0.41235 18.6548 0.130124 24.3585C2.79599 26.2959 5.36889 27.4739 7.89682 28.2489C8.51679 27.4119 9.07477 26.5129 9.55525 25.5675C8.64079 25.2265 7.77283 24.808 6.93587 24.312C7.15286 24.1571 7.36986 23.9866 7.57135 23.8161C12.6241 26.1255 18.0969 26.1255 23.0876 23.8161C23.3046 23.9866 23.5061 24.1571 23.7231 24.312C22.8861 24.808 22.0182 25.2265 21.1037 25.5675C21.5842 26.5129 22.1422 27.4119 22.7621 28.2489C25.2885 27.4739 27.8769 26.2959 30.5288 24.3585C31.1952 17.7559 29.4733 12.0212 26.0015 6.9529ZM10.2527 20.8402C8.73376 20.8402 7.49382 19.4608 7.49382 17.7714C7.49382 16.082 8.70276 14.7025 10.2527 14.7025C11.7871 14.7025 13.0425 16.082 13.0115 17.7714C13.0115 19.4608 11.7871 20.8402 10.2527 20.8402ZM20.4373 20.8402C18.9183 20.8402 17.6768 19.4608 17.6768 17.7714C17.6768 16.082 18.8873 14.7025 20.4373 14.7025C21.9717 14.7025 23.2271 16.082 23.1961 17.7714C23.1961 19.4608 21.9872 20.8402 20.4373 20.8402Z'></path>
  </svg>
);

export const TwitterIcon = () => (
  <svg width='24' height='24' fill='currentColor' viewBox='0 0 335 276'>
    <title>Twitter</title>
    <path d='m302 70a195 195 0 0 1 -299 175 142 142 0 0 0 97 -30 70 70 0 0 1 -58 -47 70 70 0 0 0 31 -2 70 70 0 0 1 -57 -66 70 70 0 0 0 28 5 70 70 0 0 1 -18 -90 195 195 0 0 0 141 72 67 67 0 0 1 116 -62 117 117 0 0 0 43 -17 65 65 0 0 1 -31 38 117 117 0 0 0 39 -11 65 65 0 0 1 -32 35'></path>
  </svg>
);

const Navbar = () => {

  function scrollToTargetAdjusted() {
    var element = document.getElementById('submission');
    var headerOffset = 75;
    var elementPosition = element.getBoundingClientRect().top;
    var offsetPosition = elementPosition + window.pageYOffset - headerOffset;

    window.scrollTo({
      top: offsetPosition,
      behavior: 'smooth',
    });
  }

  const Logo = () => (
    <div className='flex flex-shrink-0 items-center'>
      <a href='https://www.wasp-lang.dev'>
        <img src={waspLogo} width={35} height={35} alt='Wasp Logo' />
      </a>
      <span className='hidden md:block ml-3 font-semibold text-lg text-neutral-700'>
        Wasp <sup className='text-base text-yellow-500'>βetathon</sup>
      </span>
      <span className='sm:hidden text-base ml-3 font-semibold text-yellow-500'>βetathon</span>
    </div>
  );

  const SocialIcon = ({ Icon, url }) => (
    <a
      href={url}
      target='_blank'
      rel='noreferrer'
      className={`
        lg:flex hover:opacity-75 py-5
        hover:text-yellow-500 hover:border-yellow-500
        border-b-2 border-transparent
      `}
    >
      <Icon />
    </a>
  );

  const GitHubButton = () => (
    <a
      href='https://github.com/wasp-lang/wasp'
      target='_blank'
      rel='noreferrer'
      className={`
        px-2.5 py-1 rounded
        hover:bg-neutral-200
        transition ease-out duration-200
        lg:flex items-center space-x-2 text-xs
        group
      `}
    >
      <div
        className={`
          flex h-3 w-3 items-center justify-center
          group-hover:h-4 group-hover:w-4
          group-hover:text-yellow-500
        `}
      >
        <Star strokeWidth={2} />
      </div>
      <span className='truncate'>Star us on GitHub</span>
    </a>
  );



  return (
    <>
      <div className='sticky top-0 z-50'>
        <div className='bg-[#f5f4f0] absolute top-0 h-full w-full opacity-80'></div>
        <nav className='border-b backdrop-blur-sm'>
          <div
            className='
              relative mx-auto 
              flex justify-between
              h-16
              container lg:px-20 md:px-16 pl-2
            '
          >
            <div
              className='
                flex flex-1
                items-center justify-center
                justify-between
                
              '
            >
              <div className='flex flex-1 items-center'>
                {' '}
                {/* Navbar left side */}
                <Logo />
                <div className='pl-2 sm:ml-6 sm:space-x-4 lg:flex'>
                  <a href='https://wasp-lang.notion.site/Wasp-Betathon-f68015a68a15419e978c0648031a8634'>
                    <span
                      className={`
                        py-5 px-1
                        hover:text-yellow-500 hover:border-yellow-500
                        border-b-solid border-b-2 border-transparent
                        text-sm font-semibold
                      `}
                    >
                      Rules
                    </span>
                  </a>
                </div>
              </div>{' '}
              <div className='flex flex-1 items-center justify-center border-transparent'>
                {/* Submit Project */}
                <button onClick={scrollToTargetAdjusted} className='outline-none focus:outline-none'>
                  <span
                    className={`
                        py-5 px-1
                        border-b-2 border-transparent
                        text-base font-medium
                      `}
                  >
                    <div className='relative inline-flex items-center px-3 py-2 bg-gray-100 rounded border border-neutral-500 text-neutral-700 hover:text-neutral-400 hover:border-neutral-400 hover:animate-pulse transition ease-out duration-200'>
                      <svg
                        xmlns='http://www.w3.org/2000/svg'
                        width='16'
                        height='16'
                        viewBox='0 0 24 24'
                        fill='none'
                        stroke='currentColor'
                        strokeWidth='2'
                        strokeLinecap='round'
                        strokeLinejoin='round'
                      >
                        <polyline points='4 17 10 11 4 5'></polyline>
                        <line x1='12' y1='19' x2='20' y2='19'></line>
                      </svg>{' '}
                      <span className='pl-2 gradient-text hover:text-neutral-400 transition ease-out duration-200'>
                        Submit a Project
                      </span>
                    </div>
                  </span>
                </button>
              </div>{' '}
              {/* EOF left side */}
              <div className='hidden md:flex flex-1 items-center justify-end gap-2 space-x-2'>
                {' '}
                {/* Navbar right side */}
                <GitHubButton />
                <SocialIcon Icon={DiscordIcon} url='https://discord.gg/rzdnErX' />
                <SocialIcon Icon={TwitterIcon} url='https://twitter.com/WaspLang' />
              </div>{' '}
              {/* EOF right side */}
            </div>
          </div>
        </nav>
      </div>
    </>
  );
};

export default Navbar;
